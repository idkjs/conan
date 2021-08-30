type default =
  | Default;

type clear =
  | Clear;

type unsigned = {unsigned: bool};

type endian = [ | `BE | `LE | `ME | `NE];

type t('test, 'v) =
  pri
    | Default: t(default, default)
    | Regex({
        case_insensitive: bool,
        start: bool,
        limit: int64,
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
    | Pascal_string: t(string, string)
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

let serialize: (Format.formatter, t('test, 'v)) => unit;

let pp: (Format.formatter, t('test, 'v)) => unit;

let pp_of_result: (t('test, 'v), Format.formatter, 'v) => unit;

let default: t(default, default);

let clear: t(clear, clear);

let regex:
  (
    ~case_insensitive: bool=?,
    ~start: bool=?,
    ~limit: int64=?,
    [ | `Line | `Byte]
  ) =>
  t(Re.t, Ropes.t);

let pascal_string: t(string, string);

let search:
  (
    ~compact_whitespaces: bool=?,
    ~optional_blank: bool=?,
    ~lower_case_insensitive: bool=?,
    ~upper_case_insensitive: bool=?,
    [ | `Text | `Binary],
    ~trim: bool=?,
    int64,
    ~pattern: string
  ) =>
  t(string, string);

let with_range: (int64, t(string, string)) => t(string, string);

let with_pattern: (string, t(string, string)) => t(string, string);

let str_unicode: [ | `BE | `LE] => t(string, string);

let numeric:
  (~unsigned: bool=?, ~endian: endian=?, Integer.t('w), Arithmetic.t('w)) =>
  t('w, 'w);

let date:
  (
    [ | `Date | `Ldate | `Qdate | `Qldate | `Qwdate],
    option([ | `BE | `LE | `ME]),
    Arithmetic.t(Ptime.Span.t)
  ) =>
  t(Ptime.t, string);

let float:
  (~unsigned: bool=?, ~endian: endian=?, Arithmetic.t(float)) =>
  t(float, float);

let double:
  (~unsigned: bool=?, ~endian: endian=?, Arithmetic.t(float)) =>
  t(float, float);

let indirect: [ | `Rel | `Abs] => t(_, _);

open Sigs;

let process:
  (scheduler('s), syscall('fd, 'error, 's), 'fd, int64, t('test, 'v)) =>
  io(result('v, [> | `Syscall('error) | `Invalid_date | `Not_found]), 's);
