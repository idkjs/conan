open Conan;
open Sigs;

let error_msgf = fmt => Format.kasprintf(err => Error(`Msg(err)), fmt);

module Make = (S: {type t(+'a);}) => {
  type t;

  external prj: io('a, t) => S.t('a) = "%identity";

  external inj: S.t('a) => io('a, t) = "%identity";
};

module Unix_scheduler =
  Make({
    type t(+'a) = 'a;
  });

let unix =
  Unix_scheduler.{bind: (x, f) => f(prj(x)), return: x => inj(x)};

external get_uint16: (string, int) => int = "%caml_string_get16";

external get_uint32: (string, int) => int32 = "%caml_string_get32";

external get_uint64: (string, int) => int64 = "%caml_string_get64";

module File = {
  type t = {
    table: Weak.t((int64, int, string)),
    fd: Unix.file_descr,
    mutable cur: int,
    mutable seek: int64,
    max: int64,
    tmp: bytes /* XXX(dinosaure): only for line. */
  };

  let openfile = filename => {
    let fd = Unix.openfile(filename, Unix.[O_RDONLY], 0o644);
    let {Unix.LargeFile.st_size: max, _} = Unix.LargeFile.fstat(fd);
    let table = Weak.create(0xff + 1);
    {table, seek: 0L, fd, cur: 0, max, tmp: Bytes.create(80)};
  };

  let close = t => Unix.close(t.fd);

  let seek = (t, offset) =>
    fun
    | Sigs.SET =>
      if (offset < t.max) {
        t.seek = offset;
        Ok();
      } else {
        Error(`Out_of_bound);
      }
    | Sigs.CUR =>
      if (Int64.add(t.seek, offset) < t.max) {
        t.seek = Int64.add(t.seek, offset);
        Ok();
      } else {
        Error(`Out_of_bound);
      }
    | Sigs.END =>
      if (Int64.add(t.max, offset) > 0L) {
        t.seek = Int64.sub(t.max, offset);
        Ok();
      } else {
        Error(`Out_of_bound);
      };

  let load = (~seek, t) => {
    let off = Int64.(mul(div(seek, 255L), 255L));
    let off = Unix.LargeFile.lseek(t.fd, off, Unix.SEEK_SET);
    let len = min(Int64.sub(t.max, off), 255L);
    let len = Int64.to_int(len);
    if (off < 0L || len == 0) {
      None;
    } else {
      try({
        let buf = Bytes.create(len);
        let _ = Unix.read(t.fd, buf, 0, len);
        let cell = (off, len, Bytes.unsafe_to_string(buf));
        Weak.set(t.table, t.cur land 0xff, Some(cell));
        t.cur = t.cur + 1;
        Some(cell);
      }) {
      | _ => None
      };
    };
  };

  exception Found((int64, int, string));

  let find = (~seek, t) =>
    if (seek >= t.max || seek < 0L) {
      None;
    } else {
      try(
        {
          for (i in 0 to Weak.length(t.table) - 1) {
            switch (Weak.get(t.table, i)) {
            | Some((off, len, payload)) =>
              if (seek >= off && len - Int64.(to_int(sub(seek, off))) > 0) {
                raise_notrace([@implicit_arity] Found(off, len, payload));
              }
            | _ => ()
            };
          };
          load(~seek, t);
        }
      ) {
      | [@implicit_arity] Found(off, len, payload) =>
        Some((off, len, payload))
      };
    };

  let read = (t, required) =>
    if (required < 256) {
      let rec go = (acc, ~seek, required) =>
        switch (find(~seek, t)) {
        | Some((off, len, payload)) =>
          let sub_off = Int64.to_int(Int64.sub(seek, off));
          let sub_len = len - sub_off;

          if (sub_len >= required) {
            List.rev([(sub_off, sub_len, payload), ...acc]);
          } else {
            go(
              [(sub_off, sub_len, payload), ...acc],
              ~seek=Int64.(add(seek, of_int(sub_len))),
              required - sub_len,
            );
          };
        | None => List.rev(acc)
        };
      let ps = go([], ~seek=t.seek, required);
      let buf = Buffer.create(255);
      let rec concat =
        fun
        | [] =>
          if (Buffer.length(buf) > 0) {
            Some((0, Buffer.length(buf), Buffer.contents(buf)));
          } else {
            None;
          }
        | [(off, len, payload), ...rest] => {
            Buffer.add_substring(buf, payload, off, len);
            concat(rest);
          };
      concat(ps);
    } else {
      let buf = Bytes.create(required);
      let _ = Unix.LargeFile.lseek(t.fd, t.seek, Unix.SEEK_SET);
      let _ = Unix.read(t.fd, buf, 0, required);
      Some((0, required, Bytes.unsafe_to_string(buf)));
    };

  let read_int8 = t =>
    switch (read(t, 1)) {
    | Some((off, _, payload)) => Ok(Char.code(payload.[off]))
    | None => Error(`Out_of_bound)
    };

  let read_int16_ne = t =>
    switch (read(t, 2)) {
    | Some((off, len, payload)) when len >= 2 =>
      Ok(get_uint16(payload, off))
    | _ => Error(`Out_of_bound)
    };

  let read_int32_ne = t =>
    switch (read(t, 4)) {
    | Some((off, len, payload)) when len >= 4 =>
      Ok(get_uint32(payload, off))
    | _ => Error(`Out_of_bound)
    };

  let read_int64_ne = t =>
    switch (read(t, 8)) {
    | Some((off, len, payload)) when len >= 8 =>
      Ok(get_uint64(payload, off))
    | _ => Error(`Out_of_bound)
    };

  let rec index = (buf, chr, pos, limit) => {
    if (pos >= limit) {
      raise(Not_found);
    };
    if (Bytes.unsafe_get(buf, pos) == chr) {
      pos;
    } else {
      index(buf, chr, succ(pos), limit);
    };
  };

  let index = (buf, chr, ~off, ~len) =>
    index(buf, chr, off, off + len) - off;

  let line = t => {
    let len' = Unix.read(t.fd, t.tmp, 0, Bytes.length(t.tmp));
    try({
      let pos = index(t.tmp, '\n', ~off=0, ~len=len');
      t.seek = Int64.add(t.seek, Int64.of_int(pos));
      [@implicit_arity] Ok(0, pos, Bytes.unsafe_to_string(t.tmp));
    }) {
    | _ => Error(`Out_of_bound)
    };
  };

  let read = (t, required) =>
    switch (read(t, required)) {
    | Some((off, len, payload)) =>
      if (len >= required) {
        Ok(String.sub(payload, off, required));
      } else {
        Error(`Out_of_bound);
      }
    | None => Error(`Out_of_bound)
    };

  let syscall =
    Unix_scheduler.{
      seek: (f, p, w) => inj(seek(f, p, w)),
      read: (f, l) => inj(read(f, l)),
      read_int8: f => inj(read_int8(f)),
      read_int16_ne: f => inj(read_int16_ne(f)),
      read_int32_ne: f => inj(read_int32_ne(f)),
      read_int64_ne: f => inj(read_int64_ne(f)),
      line: f => inj(line(f)),
    };
};

let (/) = Filename.concat;

let fill_tree = database => {
  let files = Sys.readdir(database);
  let files = Array.to_list(files);
  let rec go = tree =>
    fun
    | [] => tree
    | [filename, ...rest] => {
        let ic = open_in(database / filename);
        let rs = Parse.parse_in_channel(ic);
        close_in(ic);
        switch (rs) {
        | Ok(lines) =>
          let (_, tree) =
            List.fold_left(
              ((line, tree), v) =>
                (succ(line), Tree.append(~filename, ~line, tree, v)),
              (1, tree),
              lines,
            );
          go(tree, rest);
        | _ => go(tree, rest)
        };
      };
  go(Tree.empty, files);
};

let database = (~directory) => fill_tree(directory);

let run_with_tree = (tree, filename) => {
  let database = Process.database(~tree);
  let result = {
    let fd = File.openfile(filename);
    let rs =
      Unix_scheduler.prj(
        Process.descending_walk(unix, File.syscall, fd, database),
      );

    File.close(fd);
    rs;
  };
  Ok(result);
};

let run = (~database, filename) =>
  if (Sys.file_exists(filename)) {
    let tree = fill_tree(database);
    let database = Process.database(~tree);
    let result = {
      let fd = File.openfile(filename);
      let rs =
        Unix_scheduler.prj(
          Process.descending_walk(unix, File.syscall, fd, database),
        );
      File.close(fd);
      rs;
    };
    Ok(result);
  } else if (Sys.file_exists(filename)) {
    error_msgf("%s does not exist", database);
  } else {
    error_msgf("%s does not exist", filename);
  };
