type default =
  | Default;

type clear =
  | Clear;

type unsigned = {unsigned: bool};

type endian = [ | `BE | `LE | `ME | `NE];

type t('test, 'v) =
  | Default: t(default, default)
  | Regex({
      case_insensitive: bool,
      start: bool,
      limit: int64,
      /* default: 8KiB */
      kind: [ | `Byte | `Line],
    })
    : t(Re.t, Ropes.t)
  | Clear: t(clear, clear)
  | Search({
      compact_whitespaces: bool,
      optional_blank: bool,
      lower_case_insensitive: bool,
      upper_case_insensitive: bool,
      text: bool,
      binary: bool,
      trim: bool,
      range: int64,
      pattern: string,
      find: Kmp.finder,
    })
    : t(string, string)
  | Pascal_string /* uh?! */: t(string, string)
  | Unicode_string([ | `BE | `LE]): t(string, string)
  | Byte(unsigned, Arithmetic.t(char)): t(char, char)
  | Short(unsigned, Arithmetic.t(int), [ | `BE | `LE | `NE]): t(int, int)
  | Long(unsigned, Arithmetic.t(int32), endian): t(int32, int32)
  | Quad(unsigned, Arithmetic.t(int64), endian): t(int64, int64)
  | Float(unsigned, Arithmetic.t(float), endian): t(float, float)
  | Double(unsigned, Arithmetic.t(float), endian): t(float, float)
  | Indirect([ | `Rel | `Abs]): t('test, 'v)
  | Date(
      [ | `Local | `UTC | `Window],
      [ | `s32 | `s64],
      Arithmetic.t(Ptime.span),
      endian,
    )
    : t(Ptime.t, string);

let serialize_endian = ppf =>
  fun
  | `BE => Format.pp_print_string(ppf, "`BE")
  | `LE => Format.pp_print_string(ppf, "`LE")
  | `ME => Format.pp_print_string(ppf, "`ME")
  | `NE => Format.pp_print_string(ppf, "`NE");

let serialize: type test v. (Format.formatter, t(test, v)) => unit =
  ppf =>
    fun
    | Default => Format.pp_print_string(ppf, "Conan.Ty.default")
    | Regex({case_insensitive, start, limit, kind}) => {
        let serialize_kind = ppf => (
          fun
          | `Byte => Format.fprintf(ppf, "`Byte")
          | `Line => Format.fprintf(ppf, "`Line")
        );
        Format.fprintf(
          ppf,
          "@[<2>Conan.Ty.regex@ ~case_insensitive:%b ~start:%b@ ~limit:%LdL@ %a@]",
          case_insensitive,
          start,
          limit,
          serialize_kind,
          kind,
        );
      }
    | Clear => Format.pp_print_string(ppf, "Conan.Ty.clear")
    | Search({
        compact_whitespaces,
        optional_blank,
        lower_case_insensitive,
        upper_case_insensitive,
        text,
        binary,
        trim,
        range,
        pattern,
        find: _,
      }) => {
        let serialize_type = (ppf, ()) =>
          switch (text, binary) {
          | (true, false) => Format.pp_print_string(ppf, "`Text")
          | (false, true) => Format.pp_print_string(ppf, "`Binary")
          | _ => assert(false)
          } /* XXX(dinosaure): should never occur! */;
        Format.fprintf(
          ppf,
          "@[<2>Conan.Ty.search@ ~compact_whitespaces:%b@ ~optional_blank:%b@ ~lower_case_insensitive:%b@ ~upper_case_insensitive:%b@ @[%a@]@ ~trim:%b@ %LdL@ ~pattern:%S@]",
          compact_whitespaces,
          optional_blank,
          lower_case_insensitive,
          upper_case_insensitive,
          serialize_type,
          (),
          trim,
          range,
          pattern,
        );
      }
    | Pascal_string => Format.pp_print_string(ppf, "Conan.Ty.pascal_string")
    | Unicode_string(`BE) =>
      Format.fprintf(ppf, "@[<2>Conan.Ty.str_unicode@ `BE@]")
    | Unicode_string(`LE) =>
      Format.fprintf(ppf, "@[<2>Conan.Ty.str_unicode@ `LE@]")
    | [@implicit_arity] Byte({unsigned}, arithmetic) =>
      Format.fprintf(
        ppf,
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ Conan.Integer.byte@ @[%a@]@]",
        unsigned,
        Serialize.(parens(Arithmetic.serialize(char))),
        arithmetic,
      )
    | [@implicit_arity] Short({unsigned}, arithmetic, endian) => {
        let serialize_endian = ppf => (
          fun
          | `BE => Format.pp_print_string(ppf, "`BE")
          | `LE => Format.pp_print_string(ppf, "`LE")
          | `NE => Format.pp_print_string(ppf, "`NE")
        );
        Format.fprintf(
          ppf,
          "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.short @[%a@]@]",
          unsigned,
          serialize_endian,
          endian,
          Serialize.(parens(Arithmetic.serialize(int))),
          arithmetic,
        );
      }
    | [@implicit_arity] Long({unsigned}, arithmetic, endian) =>
      Format.fprintf(
        ppf,
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.int32 @[%a@]@]",
        unsigned,
        serialize_endian,
        endian,
        Serialize.(parens(Arithmetic.serialize(int32))),
        arithmetic,
      )
    | [@implicit_arity] Quad({unsigned}, arithmetic, endian) =>
      Format.fprintf(
        ppf,
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.int64 @[%a@]@]",
        unsigned,
        serialize_endian,
        endian,
        Serialize.(parens(Arithmetic.serialize(int64))),
        arithmetic,
      )
    | [@implicit_arity] Float({unsigned}, arithmetic, endian) =>
      Format.fprintf(
        ppf,
        "@[<2>Conan.Ty.float@ ~unsigned:%b@ ~endian:%a@ @[%a@]@]",
        unsigned,
        serialize_endian,
        endian,
        Serialize.(parens(Arithmetic.serialize(float))),
        arithmetic,
      )
    | [@implicit_arity] Double({unsigned}, arithmetic, endian) =>
      Format.fprintf(
        ppf,
        "@[<2>Conan.Ty.double@ ~unsigned:%b@ ~endian:%a@ @[%a@]@]",
        unsigned,
        serialize_endian,
        endian,
        Serialize.(parens(Arithmetic.serialize(float))),
        arithmetic,
      )
    | Indirect(`Rel) => Format.fprintf(ppf, "@[<2>Conan.Ty.indirect@ `Rel@]")
    | Indirect(`Abs) => Format.fprintf(ppf, "@[<2>Conan.Ty.indirect@ `Abs@]")
    | [@implicit_arity] Date(zone, size, arithmetic, endian) => {
        let serialize_type = (ppf, ()) =>
          switch (zone, size) {
          | (`UTC, `s32) => Format.pp_print_string(ppf, "`Date")
          | (`Local, `s32) => Format.pp_print_string(ppf, "`Ldate")
          | (`UTC, `s64) => Format.pp_print_string(ppf, "`Qdate")
          | (`Local, `s64) => Format.pp_print_string(ppf, "`Qldate")
          | (`Window, `s64) => Format.pp_print_string(ppf, "`Qwdate")
          | (`Window, `s32) => assert(false)
          } /* XXX(dinosaure): should never occur! */;
        Format.fprintf(
          ppf,
          "@[<2>Conan.Ty.date@ @[%a@]@ @[<1>(Some@ %a)@]@ @[%a@]@]",
          serialize_type,
          (),
          serialize_endian,
          endian,
          Serialize.(parens(Arithmetic.serialize(parens(ptime_span)))),
          arithmetic,
        );
      };

let pf = Format.fprintf;

let pp_unsigned = (ppf, {unsigned}) =>
  if (unsigned) {
    pf(ppf, "u");
  };

let pp_flag = (letter, ppf) =>
  fun
  | true => pf(ppf, "%c", letter)
  | false => ();

let pp_int = ppf => pf(ppf, "0x%x");

let pp_int32 = ppf => pf(ppf, "0x%lx");

let pp_int64 = ppf => pf(ppf, "0x%Lx");

let pp_ptime = (ppf, v) =>
  switch (Ptime.Span.to_int_s(v)) {
  | Some(v) => pf(ppf, "%d", v)
  | None => pf(ppf, "%.0f", Ptime.Span.to_float_s(v))
  };

let pp_float = ppf => pf(ppf, "%f");

let pp_endian = ppf =>
  fun
  | `BE => pf(ppf, "be")
  | `LE => pf(ppf, "le")
  | `ME => pf(ppf, "me")
  | `NE => pf(ppf, "ne");

let pp_date_size = ppf =>
  fun
  | `s32 => ()
  | `s64 => pf(ppf, "q");

let pp_of_result: type test v. (t(test, v), Format.formatter, v) => unit =
  fun
  | Default => ((ppf, Default) => Format.fprintf(ppf, "x"))
  | Regex(_) => (
      (ppf, ropes) => {
        let (str, off, len) = Ropes.to_string(ropes);
        Format.fprintf(ppf, "%S", String.sub(str, off, len));
      }
    )
  | Clear => ((ppf, Clear) => Format.fprintf(ppf, "clear"))
  | Search(_) => ((ppf, v) => Format.fprintf(ppf, "%S", v))
  | Pascal_string => ((ppf, v) => Format.fprintf(ppf, "%S", v))
  | Unicode_string(_) => ((ppf, v) => Format.fprintf(ppf, "%S", v))
  | Byte(_) => (
      (ppf, chr) => Format.fprintf(ppf, "%S", String.make(1, chr))
    )
  | Short(_) => ((ppf, v) => Format.fprintf(ppf, "%d", v))
  | Long(_) => ((ppf, v) => Format.fprintf(ppf, "%ld", v))
  | Quad(_) => ((ppf, v) => Format.fprintf(ppf, "%Ld", v))
  | Float(_) => ((ppf, v) => Format.fprintf(ppf, "%f", v))
  | Double(_) => ((ppf, v) => Format.fprintf(ppf, "%f", v))
  | Indirect(_) => ((ppf, _) => Format.fprintf(ppf, "#indirection"))
  | Date(_) => ((ppf, v) => Format.fprintf(ppf, "%S", v));

let pp: type test v. (Format.formatter, t(test, v)) => unit =
  ppf =>
    fun
    | Default => pf(ppf, "default")
    | Clear => pf(ppf, "clear")
    | Regex({
        case_insensitive: false,
        start: false,
        limit: 8192L,
        kind: `Byte,
      }) =>
      pf(ppf, "regex")
    | Regex({case_insensitive, start, limit, kind}) => {
        let line =
          switch (kind) {
          | `Byte => false
          | `Line => true
          };
        pf(
          ppf,
          "regex/%a%a%a/%Ld",
          pp_flag('c'),
          case_insensitive,
          pp_flag('s'),
          start,
          pp_flag('l'),
          line,
          limit,
        );
      }
    | Search({
        compact_whitespaces: false,
        optional_blank: false,
        lower_case_insensitive: false,
        upper_case_insensitive: false,
        text: false,
        binary: false,
        trim: false,
        range: 0L,
        pattern: "",
        find: _,
      }) =>
      pf(ppf, "search")
    | Search({
        compact_whitespaces,
        optional_blank,
        lower_case_insensitive,
        upper_case_insensitive,
        text,
        binary,
        trim,
        range,
        pattern: _,
        find: _,
      }) =>
      pf(
        ppf,
        "search/%a%a%a%a%a%a%a/%Ld",
        pp_flag('W'),
        compact_whitespaces,
        pp_flag('w'),
        optional_blank,
        pp_flag('c'),
        lower_case_insensitive,
        pp_flag('C'),
        upper_case_insensitive,
        pp_flag('b'),
        binary,
        pp_flag('t'),
        text,
        pp_flag('T'),
        trim,
        range,
      )
    | Pascal_string => pf(ppf, "pstring")
    | Unicode_string(`BE) => pf(ppf, "bestring16")
    | Unicode_string(`LE) => pf(ppf, "lestring16")
    | [@implicit_arity] Byte(unsigned, arithmetic) => {
        let pp_byte = (ppf, v) => pf(ppf, "%02x", Char.code(v));
        pf(
          ppf,
          "%abyte%a",
          pp_unsigned,
          unsigned,
          Arithmetic.pp(pp_byte),
          arithmetic,
        );
      }
    | [@implicit_arity] Short(unsigned, arithmetic, endian) =>
      pf(
        ppf,
        "%a%ashort%a",
        pp_unsigned,
        unsigned,
        pp_endian,
        endian,
        Arithmetic.pp(pp_int),
        arithmetic,
      )
    | [@implicit_arity] Long(unsigned, arithmetic, endian) =>
      pf(
        ppf,
        "%a%along%a",
        pp_unsigned,
        unsigned,
        pp_endian,
        endian,
        Arithmetic.pp(pp_int32),
        arithmetic,
      )
    | [@implicit_arity] Quad(unsigned, arithmetic, endian) =>
      pf(
        ppf,
        "%a%aquad%a",
        pp_unsigned,
        unsigned,
        pp_endian,
        endian,
        Arithmetic.pp(pp_int64),
        arithmetic,
      )
    | [@implicit_arity] Float(unsigned, arithmetic, endian) =>
      pf(
        ppf,
        "%a%afloat%a",
        pp_unsigned,
        unsigned,
        pp_endian,
        endian,
        Arithmetic.pp(pp_float),
        arithmetic,
      )
    | [@implicit_arity] Double(unsigned, arithmetic, endian) =>
      pf(
        ppf,
        "%a%adouble%a",
        pp_unsigned,
        unsigned,
        pp_endian,
        endian,
        Arithmetic.pp(pp_float),
        arithmetic,
      )
    | Indirect(`Rel) => pf(ppf, "indirect/r")
    | Indirect(`Abs) => pf(ppf, "indirect")
    | [@implicit_arity] Date(`Local, size, arithmetic, endian) =>
      pf(
        ppf,
        "%a%aldate%a",
        pp_endian,
        endian,
        pp_date_size,
        size,
        Arithmetic.pp(pp_ptime),
        arithmetic,
      )
    | [@implicit_arity] Date(`UTC, size, arithmetic, endian) =>
      pf(
        ppf,
        "%a%adate%a",
        pp_endian,
        endian,
        pp_date_size,
        size,
        Arithmetic.pp(pp_ptime),
        arithmetic,
      )
    | [@implicit_arity] Date(`Window, size, arithmetic, endian) =>
      pf(
        ppf,
        "%a%awdate%a",
        pp_endian,
        endian,
        pp_date_size,
        size,
        Arithmetic.pp(pp_ptime),
        arithmetic,
      );

let default: t(default, default) = (Default: t(default, default));

let clear: t(clear, clear) = (Clear: t(clear, clear));

let regex = (~case_insensitive=false, ~start=false, ~limit=8192L, kind) =>
  Regex({case_insensitive, start, limit, kind});

let pascal_string = Pascal_string;

let search =
    (
      ~compact_whitespaces=false,
      ~optional_blank=false,
      ~lower_case_insensitive=false,
      ~upper_case_insensitive=false,
      kind,
      ~trim=false,
      range,
      ~pattern,
    ) => {
  if (Int64.of_int(String.length(pattern)) > range) {
    invalid_arg("Pattern can not be larger than the range");
  };
  let (text, binary) =
    switch (kind) {
    | `Text => (true, false)
    | `Binary => (false, true)
    };
  Search({
    compact_whitespaces,
    optional_blank,
    lower_case_insensitive,
    upper_case_insensitive,
    text,
    binary,
    trim,
    range,
    pattern,
    find: Kmp.find_one(~pattern),
  });
};

let with_range = range =>
  fun
  | Search(v) => Search({...v, range})
  | t => t;

let with_pattern = pattern =>
  fun
  | Search(v) => Search({...v, pattern})
  | t => t;

let str_unicode = endian => Unicode_string(endian);

let system_endian =
  if (Sys.big_endian) {
    `BE;
  } else {
    `LE;
  };

let numeric:
  type w.
    (~unsigned: bool=?, ~endian: endian=?, Integer.t(w), Arithmetic.t(w)) =>
    t(w, w) =
  (~unsigned=false, ~endian=system_endian, w, a) =>
    switch (w) {
    | Integer.Byte => [@implicit_arity] Byte({unsigned: unsigned}, a)
    | Integer.Short =>
      let endian =
        switch (endian) {
        | `BE => `BE
        | `LE => `LE
        | `NE => `NE
        | _ => invalid_arg("Invalid endian for short")
        };
      [@implicit_arity] Short({unsigned: unsigned}, a, endian);
    | Integer.Int32 =>
      [@implicit_arity] Long({unsigned: unsigned}, a, endian)
    | Integer.Int64 =>
      [@implicit_arity] Quad({unsigned: unsigned}, a, endian)
    };

let date = (kind, endian, a) => {
  let endian =
    switch (endian) {
    | Some(`BE) => `BE
    | Some(`LE) => `LE
    | Some(`ME) => `ME
    | None =>
      if (Sys.big_endian) {
        `BE;
      } else {
        `LE;
      }
    };
  switch (kind) {
  | `Date => [@implicit_arity] Date(`UTC, `s32, a, endian)
  | `Ldate => [@implicit_arity] Date(`Local, `s32, a, endian)
  | `Qdate => [@implicit_arity] Date(`UTC, `s64, a, endian)
  | `Qldate => [@implicit_arity] Date(`Local, `s64, a, endian)
  | `Qwdate => [@implicit_arity] Date(`Window, `s64, a, endian)
  };
};

let float = (~unsigned=false, ~endian=system_endian, c) =>
  [@implicit_arity] Float({unsigned: unsigned}, c, endian);

let double = (~unsigned=false, ~endian=system_endian, c) =>
  [@implicit_arity] Double({unsigned: unsigned}, c, endian);

let indirect = v => Indirect(v);

open Sigs;

let (<.>) = (f, g, x) => f(g(x));

let newline = "\n"; /* TODO: windows */

let ok = x => Ok(x);

let error = err => Error(err);

let reword_error = f =>
  fun
  | Ok(x) => Ok(x)
  | Error(err) => Error(f(err));

let id = x => x;

let read_float = ({bind, return} as scheduler, syscall, fd, endian) => {
  let (>>=) = bind;
  let size =
    switch (endian) {
    | `LE => Size.lelong
    | `BE => Size.belong
    | `ME => assert(false) /* TODO */
    | `NE => Size.long
    };
  Size.read(scheduler, syscall, fd, size)
  >>= (
    fun
    | Ok(v) => {
        let v = Int64.to_int32(v);
        /* safe */
        return(Ok(Int32.float_of_bits(v)));
      }
    | Error(_) as err => return(err)
  );
};

let read_double = ({bind, return} as scheduler, syscall, fd, endian) => {
  let (>>=) = bind;
  let size =
    switch (endian) {
    | `LE => Size.lequad
    | `BE => Size.bequad
    | `ME => assert(false) /* TODO */
    | `NE => Size.quad
    };
  Size.read(scheduler, syscall, fd, size)
  >>= (
    fun
    | Ok(v) => return(Ok(Int64.float_of_bits(v)))
    | Error(_) as err => return(err)
  );
};

let process:
  type s fd error test v.
    (scheduler(s), syscall(fd, error, s), fd, int64, t(test, v)) =>
    io(result(v, [> | `Syscall(error) | `Invalid_date | `Not_found]), s) =
  ({bind, return} as scheduler, syscall, fd, abs_offset, ty) => {
    let (>>=) = bind;
    let (>?=) = (x, f) =>
      x
      >>= (
        fun
        | Ok(x) => f(x)
        | Error(err) => return(Error(err))
      );
    let (>|=) = (x, f) => x >>= (x => return(f(x)));
    switch (ty) {
    | Default => (return <.> ok)(Default: default)
    | Clear => (return <.> ok)(Clear: clear)
    | Search({pattern: "", _}) => (return <.> ok)("")
    | Search({
        pattern: _,
        compact_whitespaces: _,
        optional_blank: _,
        lower_case_insensitive: _,
        upper_case_insensitive: _,
        text: _,
        binary: _,
        trim: _,
        range,
        find,
      }) =>
      let get = (fd, ~pos) =>
        syscall.seek(fd, Int64.add(abs_offset, pos), SET)
        >?= (
          () =>
            syscall.read_int8(fd)
            >?= (code => (return <.> ok <.> Char.chr)(code))
        );

      find.Kmp.f(scheduler, ~get, ~ln=range, fd)
      >|= reword_error(err => `Syscall(err))
      >?= (
        results => {
          let results = List.sort(Int64.compare, results);
          switch (results) {
          | [] => return(Error(`Not_found))
          | [rel_offset, ..._] =>
            syscall.seek(fd, Int64.add(abs_offset, rel_offset), SET)
            >|= reword_error(err => `Syscall(err))
            >?= (
              () =>
                syscall.read(fd, Int64.to_int(range))
                >|= reword_error(err => `Syscall(err))
            )
          };
        }
      );
    | Regex({kind, limit, _}) =>
      switch (kind) {
      | `Byte =>
        syscall.read(fd, Int64.to_int(limit))
        >|= reword_error(err => `Syscall(err))
        >?= (raw => return(Ok(Ropes.of_string(raw))))
      | `Line =>
        let rec go = acc => (
          fun
          | 0 => {
              let ropes =
                Ropes.append(
                  Ropes.concat(~sep=newline, acc),
                  Ropes.of_string(newline),
                );
              return(Ok(ropes));
            }
          | n =>
            syscall.line(fd)
            >|= reword_error(err => `Syscall(err))
            >?= (
              ((off, len, line)) =>
                go([Ropes.of_string(~off, ~len, line), ...acc], pred(n))
            )
        );
        go([], max(0, Int64.to_int(limit)));
      }
    | [@implicit_arity] Byte({unsigned}, c) =>
      let (size, converter, w) = (
        Size.byte,
        Char.chr <.> Int64.to_int,
        Integer.byte,
      );
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> converter)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process(~unsigned, w, v, c)))
      );
    | [@implicit_arity] Short({unsigned}, c, endian) =>
      let (size, converter, w) =
        switch (endian) {
        | `LE => (Size.leshort, Int64.to_int, Integer.short)
        | `BE => (Size.beshort, Int64.to_int, Integer.short)
        | `NE => (Size.short, Int64.to_int, Integer.short)
        };
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> converter)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process(~unsigned, w, v, c)))
      );
    | [@implicit_arity] Long({unsigned}, c, endian) =>
      let (size, converter, w) =
        switch (endian) {
        | `LE => (Size.lelong, Int64.to_int32, Integer.int32)
        | `BE => (Size.belong, Int64.to_int32, Integer.int32)
        | `NE => (Size.long, Int64.to_int32, Integer.int32)
        | `ME => failwith("Middle-endian not supported")
        };
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> converter)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process(~unsigned, w, v, c)))
      );
    | [@implicit_arity] Quad({unsigned}, c, endian) =>
      let (size, converter, w) =
        switch (endian) {
        | `LE => (Size.lequad, id, Integer.int64)
        | `BE => (Size.bequad, id, Integer.int64)
        | `NE => (Size.quad, id, Integer.int64)
        | `ME => failwith("Middle-endian not supported")
        };
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> converter)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process(~unsigned, w, v, c)))
      );
    | [@implicit_arity] Float({unsigned: _}, c, endian) =>
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          read_float(scheduler, syscall, fd, endian)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process_float(v, c)))
      )
    | [@implicit_arity] Double({unsigned: _}, c, endian) =>
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          read_double(scheduler, syscall, fd, endian)
          >|= reword_error(err => `Syscall(err))
          >?= (v => (return <.> ok)(Arithmetic.process_float(v, c)))
      )
    | [@implicit_arity] Date(_, `s32, c, endian) =>
      let size =
        switch (endian) {
        | `BE => Size.belong
        | `LE => Size.lelong
        | `NE => Size.long
        | `ME => assert(false)
        };
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> Ptime.Span.of_int_s <.> Int64.to_int)
          >|= reword_error(err => `Syscall(err))
          >?= (
            v =>
              switch (Ptime.of_span(Arithmetic.process_ptime(v, c))) {
              | Some(v) => return(ok(Ptime.to_rfc3339(v)))
              | None => return(error(`Invalid_date))
              }
          )
      );
    | [@implicit_arity] Date(_, `s64, c, endian) =>
      let size =
        switch (endian) {
        | `BE => Size.bequad
        | `LE => Size.lequad
        | `NE => Size.quad
        | `ME => assert(false)
        };
      syscall.seek(fd, abs_offset, SET)
      >|= reword_error(err => `Syscall(err))
      >?= (
        () =>
          Size.read(scheduler, syscall, fd, size)
          >?= (return <.> ok <.> Ptime.Span.of_int_s <.> Int64.to_int)
          >|= reword_error(err => `Syscall(err))
          >?= (
            v =>
              switch (Ptime.of_span(Arithmetic.process_ptime(v, c))) {
              | Some(v) => return(ok(Ptime.to_rfc3339(v)))
              | None => return(error(`Invalid_date))
              }
          )
      );
    | Unicode_string(_) => assert(false)
    | Pascal_string => assert(false)
    | Indirect(_) => assert(false)
    };
  };
