type t =
  pri
    | Byte
    | Leshort
    | Beshort
    | Lelong
    | Belong
    | Melong
    | Leid3
    | Beid3
    | Lequad
    | Bequad;

let serialize: (Format.formatter, t) => unit;

let byte: t;

let leshort: t;

let beshort: t;

let short: t;

let lelong: t;

let belong: t;

let melong: t;

let long: t;

let leid3: t;

let beid3: t;

let id3: t;

let lequad: t;

let bequad: t;

let quad: t;

let of_string: string => t;

let is_size: char => bool;

let pp: (Format.formatter, t) => unit;

open Sigs;

let invert:
  (scheduler('s), syscall('fd, 'error, 's)) => syscall('fd, 'error, 's);

let read:
  (scheduler('s), syscall('fd, 'error, 's), 'fd, t) =>
  io(result(int64, 'error), 's);
