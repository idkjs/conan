type t('a) =
  | Byte: t(char)
  | Short: t(int)
  | Int32: t(int32)
  | Int64: t(int64);

let serialize: type a. (Format.formatter, t(a)) => unit =
  ppf =>
    fun
    | Byte => Format.pp_print_string(ppf, "Conan.Integer.byte")
    | Short => Format.pp_print_string(ppf, "Conan.Integer.short")
    | Int32 => Format.pp_print_string(ppf, "Conan.Integer.int32")
    | Int64 => Format.pp_print_string(ppf, "Conan.Integer.int64");

let serializer_of: type a. (t(a), Format.formatter, a) => unit =
  fun
  | Byte => Serialize.char
  | Short => Serialize.int
  | Int32 => Serialize.int32
  | Int64 => Serialize.int64;

let byte = Byte;

let short = Short;

let int32 = Int32;

let int64 = Int64;

let pf = Format.fprintf;

let pp: type a. (t(a), Format.formatter, a) => unit =
  fun
  | Byte => ((ppf, v) => pf(ppf, "%02x", Char.code(v)))
  | Short => ((ppf, v) => pf(ppf, "0x%x", v))
  | Int32 => ((ppf, v) => pf(ppf, "0x%lx", v))
  | Int64 => ((ppf, v) => pf(ppf, "0x%Lx", v));

let add: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr((code(a) + code(b)) land 0xff)))
  | Short => ((a, b) => (a + b) land 0xffff)
  | Int32 => Int32.add
  | Int64 => Int64.add;

let sub: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr((code(a) + code(b)) land 0xff)))
  | Short => ((a, b) => (a - b) land 0xffff)
  | Int32 => Int32.sub
  | Int64 => Int64.sub;

let mul: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr(code(a) * code(b) land 0xff)))
  | Short => ((a, b) => a * b land 0xffff)
  | Int32 => Int32.mul
  | Int64 => Int64.mul;

let div: type w. (~unsigned: bool=?, t(w), w, w) => w =
  (~unsigned=false) =>
    fun
    | Byte => ((a, b) => Char.(chr(code(a) / code(b) land 0xff)))
    | Short => ((a, b) => a / b land 0xffff)
    | Int32 => if (unsigned) {Int32.unsigned_div} else {Int32.div}
    | Int64 => if (unsigned) {Int64.unsigned_div} else {Int64.div};

let rem: type w. (~unsigned: bool=?, t(w), w, w) => w =
  (~unsigned=false) =>
    fun
    | Byte => ((a, b) => Char.(chr(code(a) mod code(b) land 0xff)))
    | Short => ((a, b) => a mod b land 0xffff)
    | Int32 => if (unsigned) {Int32.unsigned_rem} else {Int32.rem}
    | Int64 => if (unsigned) {Int64.unsigned_rem} else {Int64.rem};

let bitwise_and: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr(code(a) land code(b) land 0xff)))
  | Short => ((a, b) => a land b land 0xffff)
  | Int32 => Int32.logand
  | Int64 => Int64.logand;

let bitwise_xor: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr(code(a) lxor code(b) land 0xff)))
  | Short => ((a, b) => a lxor b land 0xffff)
  | Int32 => Int32.logxor
  | Int64 => Int64.logxor;

let bitwise_or: type w. (t(w), w, w) => w =
  fun
  | Byte => ((a, b) => Char.(chr(code(a) lor code(b) land 0xff)))
  | Short => ((a, b) => a lor b land 0xffff)
  | Int32 => Int32.logor
  | Int64 => Int64.logor;

let invert: type w. (t(w), w) => w =
  fun
  | Byte => (v => Char.(chr(lnot(code(v)) land 0xff)))
  | Short => (v => lnot(v) land 0xffff)
  | Int32 => Int32.lognot
  | Int64 => Int64.lognot;

let greater: type w. (t(w), w, w) => bool =
  fun
  | Byte => (>)
  | Short => (>)
  | Int32 => (>)
  | Int64 => (>);

let lower: type w. (t(w), w, w) => bool =
  fun
  | Byte => (<)
  | Short => (<)
  | Int32 => (<)
  | Int64 => (<);

let equal: type w. (t(w), w, w) => bool =
  fun
  | Byte => (==)
  | Short => (==)
  | Int32 => (==)
  | Int64 => (==);

let different: type w. (t(w), w, w) => bool =
  fun
  | Byte => (!=)
  | Short => (!=)
  | Int32 => (!=)
  | Int64 => (!=);

let zero: type w. t(w) => w =
  fun
  | Byte => '\000'
  | Short => 0
  | Int32 => Int32.zero
  | Int64 => Int64.zero;

open Sub;

let parse = s => {
  let v = to_string(s);
  let lexbuf = Lexing.from_string(v);
  if (is_empty(s)) {
    Error(`Empty);
  } else {
    switch (Lexer.int(lexbuf)) {
    | Ok(v) =>
      let first = Lexing.lexeme_end(lexbuf);
      [@implicit_arity] Ok(Int64.of_string(v), with_range(~first, s));
    | Error(`Malformed) => Error(`Invalid_integer(v))
    | exception _ => Error(`Invalid_integer(v))
    };
  };
};
