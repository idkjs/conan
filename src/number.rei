type t;

let int64: int64 => t;

let float: float => t;

let pp: (Format.formatter, t) => unit;

let to_float: t => float;

let to_int: t => int;

let to_int64: t => int64;

let to_int32: t => int32;

let to_short: t => int;

let to_byte: t => char;

let to_ptime: t => Ptime.Span.t;

let parse:
  Sub.t => result((t, Sub.t), [> | `Empty | `Invalid_number(string)]);
