let invalid_arg = fmt => Format.kasprintf(invalid_arg, fmt);

type t('a) =
  | Invert(t('a))
  | Add('a)
  | Sub('a)
  | Mul('a)
  | Div('a)
  | Mod('a)
  | Bitwise_and('a)
  | Bitwise_xor('a)
  | Bitwise_or('a);

let rec serialize = (pp, ppf) =>
  fun
  | Invert(t) =>
    Format.fprintf(
      ppf,
      "Conan.Arithmetic.Invert@ @[%a@]",
      Serialize.parens(serialize(pp)),
      t,
    )
  | Add(v) => Format.fprintf(ppf, "Conan.Arithmetic.Add@ @[%a@]", pp, v)
  | Sub(v) => Format.fprintf(ppf, "Conan.Arithmetic.Sub@ @[%a@]", pp, v)
  | Mul(v) => Format.fprintf(ppf, "Conan.Arithmetic.Mul@ @[%a@]", pp, v)
  | Div(v) => Format.fprintf(ppf, "Conan.Arithmetic.Div@ @[%a@]", pp, v)
  | Mod(v) => Format.fprintf(ppf, "Conan.Arithmetic.Mod@ @[%a@]", pp, v)
  | Bitwise_and(v) =>
    Format.fprintf(ppf, "Conan.Arithmetic.Bitwise_and@ @[%a@]", pp, v)
  | Bitwise_xor(v) =>
    Format.fprintf(ppf, "Conan.Arithmetic.Bitwise_xor@ @[%a@]", pp, v)
  | Bitwise_or(v) =>
    Format.fprintf(ppf, "Conan.Arithmetic.Bitwise_or@ @[%a@]", pp, v);

let pf = Format.fprintf;

let rec pp = (pp_val, ppf) =>
  fun
  | Invert(v) => pf(ppf, "~%a", pp(pp_val), v)
  | Add(v) => pf(ppf, "+%a", pp_val, v)
  | Sub(v) => pf(ppf, "-%a", pp_val, v)
  | Mul(v) => pf(ppf, "*%a", pp_val, v)
  | Div(v) => pf(ppf, "/%a", pp_val, v)
  | Mod(v) => pf(ppf, "%%%a", pp_val, v)
  | Bitwise_and(v) => pf(ppf, "&%a", pp_val, v)
  | Bitwise_xor(v) => pf(ppf, "^%a", pp_val, v)
  | Bitwise_or(v) => pf(ppf, "|%a", pp_val, v);

let rec map = (~f) =>
  fun
  | Invert(c) => Invert(map(~f, c))
  | Add(v) => Add(f(v))
  | Sub(v) => Sub(f(v))
  | Mul(v) => Mul(f(v))
  | Div(v) => Div(f(v))
  | Mod(v) => Mod(f(v))
  | Bitwise_and(v) => Bitwise_and(f(v))
  | Bitwise_xor(v) => Bitwise_xor(f(v))
  | Bitwise_or(v) => Bitwise_or(f(v));

let rec value =
  fun
  | Invert(c) => value(c)
  | Add(v) => v
  | Sub(v) => v
  | Mul(v) => v
  | Div(v) => v
  | Mod(v) => v
  | Bitwise_and(v) => v
  | Bitwise_xor(v) => v
  | Bitwise_or(v) => v;

let of_string = (~with_val) =>
  fun
  | "+" => Add(with_val)
  | "-" => Sub(with_val)
  | "*" => Mul(with_val)
  | "/" => Div(with_val)
  | "%" => Mod(with_val)
  | "&" => Bitwise_and(with_val)
  | "^" => Bitwise_xor(with_val)
  | "|" => Bitwise_or(with_val)
  | v => invalid_arg("Invalid arithmetic operator: %S", v);

let is =
  fun
  | '+'
  | '-'
  | '*'
  | '/'
  | '%'
  | '&'
  | '|'
  | '^' => true
  | _ => false;

let add = v => Add(v);

let sub = v => Sub(v);

let mul = v => Mul(v);

let div = v => Div(v);

let rem = v => Mod(v);

let logand = v => Bitwise_and(v);

let logxor = v => Bitwise_xor(v);

let invert = v => Invert(v);

let logor = v => Bitwise_or(v);

let rec process: type a. (~unsigned: bool=?, Integer.t(a), a, t(a)) => a =
  (~unsigned=false, w, a) =>
    fun
    | Add(b) => Integer.add(w, a, b)
    | Sub(b) => Integer.sub(w, a, b)
    | Mul(b) => Integer.mul(w, a, b)
    | Div(b) => Integer.div(~unsigned, w, a, b)
    | Mod(b) => Integer.rem(~unsigned, w, a, b)
    | Bitwise_and(b) => Integer.bitwise_and(w, a, b)
    | Bitwise_xor(b) => Integer.bitwise_xor(w, a, b)
    | Bitwise_or(b) => Integer.bitwise_or(w, a, b)
    | Invert(c) => Integer.invert(w, process(w, a, c));

let process_float = a =>
  fun
  | Add(b) => Float.add(a, b)
  | Sub(b) => Float.sub(a, b)
  | Mul(b) => Float.mul(a, b)
  | Div(b) => Float.div(a, b)
  | Mod(b) => Float.rem(a, b)
  | Bitwise_and(_)
  | Bitwise_xor(_)
  | Bitwise_or(_)
  | Invert(_) => invalid_arg("Invalid bitwise operation on float");

let rec process_ptime = a =>
  fun
  | Add(b) => Ptime.Span.add(a, b)
  | Sub(b) => Ptime.Span.sub(a, b)
  | Invert(c) => Ptime.Span.neg(process_ptime(a, c))
  | Mul(_)
  | Div(_)
  | Mod(_)
  | Bitwise_and(_)
  | Bitwise_xor(_)
  | Bitwise_or(_) => invalid_arg("Invalid operation on date");
