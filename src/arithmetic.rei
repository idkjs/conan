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

let serialize:
  ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;

let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;

let map: (~f: 'a => 'b, t('a)) => t('b);

let value: t('a) => 'a;

let of_string: (~with_val: 'a, string) => t('a);

let is: char => bool;

let add: 'a => t('a);

let sub: 'a => t('a);

let div: 'a => t('a);

let rem: 'a => t('a);

let mul: 'a => t('a);

let logand: 'a => t('a);

let logxor: 'a => t('a);

let logor: 'a => t('a);

let invert: t('a) => t('a);

let process: (~unsigned: bool=?, Integer.t('a), 'a, t('a)) => 'a;

let process_float: (float, t(float)) => float;

let process_ptime: (Ptime.Span.t, t(Ptime.Span.t)) => Ptime.Span.t;
