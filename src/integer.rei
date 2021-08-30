type t('a) =
  pri | Byte: t(char) | Short: t(int) | Int32: t(int32) | Int64: t(int64);

let serialize: (Format.formatter, t('a)) => unit;

let serializer_of: (t('a), Format.formatter, 'a) => unit;

let byte: t(char);

let short: t(int);

let int32: t(int32);

let int64: t(int64);

let pp: (t('a), Format.formatter, 'a) => unit;

let add: (t('a), 'a, 'a) => 'a;

let sub: (t('a), 'a, 'a) => 'a;

let mul: (t('a), 'a, 'a) => 'a;

let div: (~unsigned: bool=?, t('a), 'a, 'a) => 'a;

let rem: (~unsigned: bool=?, t('a), 'a, 'a) => 'a;

let bitwise_and: (t('a), 'a, 'a) => 'a;

let bitwise_xor: (t('a), 'a, 'a) => 'a;

let bitwise_or: (t('a), 'a, 'a) => 'a;

let invert: (t('a), 'a) => 'a;

let greater: (t('a), 'a, 'a) => bool;

let lower: (t('a), 'a, 'a) => bool;

let equal: (t('a), 'a, 'a) => bool;

let different: (t('a), 'a, 'a) => bool;

let zero: t('a) => 'a;

let parse:
  Sub.t => result((int64, Sub.t), [> | `Empty | `Invalid_integer(string)]);
