type t('a);

let serialize:
  ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;

let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;

let of_string: (~with_val: 'a, string) => t('a);

let is: char => bool;

let map: (~f: 'a => 'b, t('a)) => t('b);

let value: t('a) => 'a;

let equal_to: 'a => t('a);

let different_to: 'a => t('a);

let greater_than: 'a => t('a);

let lower_than: 'a => t('a);

let bitwise_and: 'a => t('a);

let bitwise_xor: 'a => t('a);

let process: (Integer.t('a), 'a, t('a)) => bool;

let process_float: (float, t(float)) => bool;

let process_string: (string, t(string)) => bool;
