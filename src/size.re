let invalid_arg = fmt => Format.kasprintf(invalid_arg, fmt);

let (<.>) = (f, g, x) => f(g(x));

let ok = x => Ok(x);

type t =
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

let serialize = ppf =>
  fun
  | Byte => Format.pp_print_string(ppf, "Conan.Size.byte")
  | Leshort => Format.pp_print_string(ppf, "Conan.Size.leshort")
  | Beshort => Format.pp_print_string(ppf, "Conan.Size.beshort")
  | Lelong => Format.pp_print_string(ppf, "Conan.Size.lelong")
  | Belong => Format.pp_print_string(ppf, "Conan.Size.belong")
  | Melong => Format.pp_print_string(ppf, "Conan.Size.melong")
  | Leid3 => Format.pp_print_string(ppf, "Conan.Size.leid3")
  | Beid3 => Format.pp_print_string(ppf, "Conan.Size.beid3")
  | Lequad => Format.pp_print_string(ppf, "Conan.Size.lequad")
  | Bequad => Format.pp_print_string(ppf, "Conan.Size.bequad");

let byte = Byte;

let leshort = Leshort;

let beshort = Beshort;

let lelong = Lelong;

let belong = Belong;

let melong = Melong;

let leid3 = Leid3;

let beid3 = Beid3;

let lequad = Lequad;

let bequad = Bequad;

let short = if (Sys.big_endian) {Beshort} else {Leshort};

let long = if (Sys.big_endian) {Belong} else {Lelong};

let id3 = if (Sys.big_endian) {Beid3} else {Leid3};

let quad = if (Sys.big_endian) {Bequad} else {Lequad};

let of_string =
  fun
  | "B"
  | "b"
  | "C"
  | "c" => Byte
  | "s"
  | "h" => Leshort
  | "S"
  | "H" => Beshort
  | "l" => Lelong
  | "L" => Belong
  | "m" => Melong
  | "i" => Leid3
  | "I" => Beid3
  | "q" => Lequad
  | "Q" => Bequad
  | v => invalid_arg("Invalid size: %S", v);

let is_size =
  fun
  | 'Q'
  | 'q'
  | 'B'
  | 'b'
  | 'C'
  | 'c'
  | 's'
  | 'h'
  | 'S'
  | 'H'
  | 'l'
  | 'L'
  | 'm'
  | 'i'
  | 'I' => true
  | _ => false;

let pp = Format.fprintf;

let pp = ppf =>
  fun
  | Byte => pp(ppf, "byte")
  | Leshort => pp(ppf, "leshort")
  | Beshort => pp(ppf, "beshort")
  | Lelong => pp(ppf, "lelong")
  | Belong => pp(ppf, "belong")
  | Melong => pp(ppf, "melong")
  | Leid3 => pp(ppf, "leid3")
  | Beid3 => pp(ppf, "beid3")
  | Lequad => pp(ppf, "lequad")
  | Bequad => pp(ppf, "bequad");

open Sigs;

external swap16: int => int = "%bswap16";

external swap32: int32 => int32 = "%bswap_int32";

external swap64: int64 => int64 = "%bswap_int64";

let invert = ({bind, return}, syscall) => {
  let (>>=) = bind;
  let (>|=) = (x, f) =>
    x
    >>= (
      fun
      | Ok(x) => return(Ok(f(x)))
      | Error(err) => return(Error(err))
    );

  let read_int16_ne = fd => syscall.read_int16_ne(fd) >|= swap16;
  let read_int32_ne = fd => syscall.read_int32_ne(fd) >|= swap32;
  let read_int64_ne = fd => syscall.read_int64_ne(fd) >|= swap64;
  {...syscall, read_int16_ne, read_int32_ne, read_int64_ne};
};

let read:
  type s fd error.
    (scheduler(s), syscall(fd, error, s), fd, t) =>
    io(result(int64, error), s) =
  ({bind, return}, syscall, fd, s) => {
    let (>>=) = bind;
    let (>?=) = (x, f) =>
      x
      >>= (
        fun
        | Ok(x) => f(x)
        | Error(err) => return(Error(err))
      );
    let (>|=) = (x, f) => x >?= (x => (return <.> ok)(f(x)));

    switch (s) {
    | Byte => syscall.read_int8(fd) >|= Int64.of_int
    | Leshort =>
      if (Sys.big_endian) {
        syscall.read_int16_ne(fd) >|= swap16 >|= Int64.of_int;
      } else {
        syscall.read_int16_ne(fd) >|= Int64.of_int;
      }
    | Beshort =>
      if (Sys.big_endian) {
        syscall.read_int16_ne(fd) >|= Int64.of_int;
      } else {
        syscall.read_int16_ne(fd) >|= swap16 >|= Int64.of_int;
      }
    | Lelong =>
      if (Sys.big_endian) {
        syscall.read_int32_ne(fd) >|= swap32 >|= Int64.of_int32;
      } else {
        syscall.read_int32_ne(fd) >|= Int64.of_int32;
      }
    | Belong =>
      if (Sys.big_endian) {
        syscall.read_int32_ne(fd) >|= Int64.of_int32;
      } else {
        syscall.read_int32_ne(fd) >|= swap32 >|= Int64.of_int32;
      }
    | Lequad =>
      if (Sys.big_endian) {
        syscall.read_int64_ne(fd) >|= swap64;
      } else {
        syscall.read_int64_ne(fd);
      }
    | Bequad =>
      if (Sys.big_endian) {
        syscall.read_int64_ne(fd) >|= swap64;
      } else {
        syscall.read_int64_ne(fd);
      }
    | s => invalid_arg("Unsupported size %a", pp, s)
    };
  };
