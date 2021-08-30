open Conan.Sigs;

module Make = (S: {type t(+'a);}) => {
  type t;

  external prj: io('a, t) => S.t('a) = "%identity";

  external inj: S.t('a) => io('a, t) = "%identity";
};

module Caml_scheduler =
  Make({
    type t(+'a) = 'a;
  });

let caml =
  Caml_scheduler.{bind: (x, f) => f(prj(x)), return: x => inj(x)};

external get_uint16: (string, int) => int = "%caml_string_get16";

external get_uint32: (string, int) => int32 = "%caml_string_get32";

external get_uint64: (string, int) => int64 = "%caml_string_get64";

module Str = {
  type t = {
    mutable seek: int,
    contents: string,
    tmp: bytes,
  };

  let openfile = str => {seek: 0, contents: str, tmp: Bytes.create(80)};

  let _max_int = Int64.of_int(max_int);

  let _min_int = Int64.of_int(min_int);

  let seek = (t, offset, seek) =>
    if (offset > _max_int || offset < _min_int) {
      Error(`Out_of_bound);
    } else {
      let offset = Int64.to_int(offset);
      switch (seek) {
      | Conan.Sigs.SET =>
        if (offset >= 0 && offset < String.length(t.contents)) {
          t.seek = offset;
          Ok();
        } else {
          Error(`Out_of_bound);
        }
      | Conan.Sigs.CUR =>
        if (t.seek + offset < String.length(t.contents)) {
          t.seek = t.seek + offset;
          Ok();
        } else {
          Error(`Out_of_bound);
        }
      | Conan.Sigs.END =>
        if (String.length(t.contents) + offset > 0) {
          t.seek = String.length(t.contents) + offset;
          Ok();
        } else {
          Error(`Out_of_bound);
        }
      };
    };

  let read = (t, required) => {
    let len = min(required, String.length(t.contents) - t.seek);
    if (len <= 0) {
      None;
    } else {
      Some(String.sub(t.contents, t.seek, len));
    };
  };

  let read_int8 = t =>
    switch (read(t, 1)) {
    | Some(str) => Ok(Char.code(str.[0]))
    | _ => Error(`Out_of_bound)
    };

  let read_int16_ne = t =>
    switch (read(t, 2)) {
    | Some(str) when String.length(str) >= 2 => Ok(get_uint16(str, 0))
    | _ => Error(`Out_of_bound)
    };

  let read_int32_ne = t =>
    switch (read(t, 4)) {
    | Some(str) when String.length(str) >= 4 => Ok(get_uint32(str, 0))
    | _ => Error(`Out_of_bound)
    };

  let read_int64_ne = t =>
    switch (read(t, 8)) {
    | Some(str) when String.length(str) >= 8 => Ok(get_uint64(str, 0))
    | _ => Error(`Out_of_bound)
    };

  let rec index = (str, chr, pos, limit) => {
    if (pos >= limit) {
      raise(Not_found);
    };
    if (str.[pos] == chr) {
      pos;
    } else {
      index(str, chr, succ(pos), limit);
    };
  };

  let index = (str, chr, ~off, ~len) =>
    index(str, chr, off, off + len) - off;

  let line = t =>
    try({
      let len = min(String.length(t.contents) - t.seek, 80);
      let off = t.seek;
      let pos = index(t.contents, '\n', ~off, ~len);
      t.seek = t.seek + (pos - off);
      [@implicit_arity] Ok(off, pos, t.contents);
    }) {
    | _ => Error(`Out_of_bound)
    };

  let read = (t, required) =>
    switch (read(t, required)) {
    | Some(str) when String.length(str) >= required => Ok(str)
    | _ => Error(`Out_of_bound)
    };

  let syscall =
    Caml_scheduler.{
      seek: (f, p, w) => inj(seek(f, p, w)),
      read: (f, l) => inj(read(f, l)),
      read_int8: f => inj(read_int8(f)),
      read_int16_ne: f => inj(read_int16_ne(f)),
      read_int32_ne: f => inj(read_int32_ne(f)),
      read_int64_ne: f => inj(read_int64_ne(f)),
      line: f => inj(line(f)),
    };
};

open Conan;

let run = (~database, contents) => {
  let result = {
    let fd = Str.openfile(contents);
    let rs =
      Caml_scheduler.prj(
        Process.descending_walk(caml, Str.syscall, fd, database),
      );

    rs;
  };
  Ok(result);
};
