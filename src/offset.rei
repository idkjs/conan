type t =
  | Relative(t)
  | Absolute(t)
  | Value(int64)
  | Read(t, Size.t)
  | Calculation(t, Arithmetic.t(t));

let serialize: (Format.formatter, t) => unit;

let pp: (Format.formatter, t) => unit;

open Sigs;

let process:
  (scheduler('s), syscall('fd, 'error, 's), 'fd, t, int64) =>
  io(result(int64, 'error), 's);
