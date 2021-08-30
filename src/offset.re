let (<.>) = (f, g, x) => f(g(x));

let ok = x => Ok(x);

type t =
  | Relative(t)
  | Absolute(t)
  | Value(int64)
  | Read(t, Size.t)
  | Calculation(t, Arithmetic.t(t));

let rec serialize = ppf =>
  fun
  | Relative(v) =>
    Format.fprintf(
      ppf,
      "Conan.Offset.Relative@ @[%a@]",
      Serialize.parens(serialize),
      v,
    )
  | Absolute(v) =>
    Format.fprintf(
      ppf,
      "Conan.Offset.Absolute@ @[%a@]",
      Serialize.parens(serialize),
      v,
    )
  | Value(v) =>
    Format.fprintf(ppf, "Conan.Offset.Value@ %a", Serialize.int64, v)
  | [@implicit_arity] Read(v, s) =>
    Format.fprintf(
      ppf,
      "Conan.Offset.Read@ @[%a@]",
      Serialize.pair(Serialize.parens(serialize), Size.serialize),
      (v, s),
    )
  | [@implicit_arity] Calculation(v, arithmetic) =>
    Format.fprintf(
      ppf,
      "Conan.Offset.Calculation@ @[%a@]",
      Serialize.pair(
        Serialize.parens(serialize),
        Arithmetic.serialize(Serialize.parens(serialize)),
      ),
      (v, arithmetic),
    );

let pf = Format.fprintf;

let rec pp = ppf =>
  fun
  | Relative(v) => pf(ppf, "&%a", pp, v)
  | Absolute(v) => pf(ppf, "%a", pp, v)
  | Value(v) => pf(ppf, "%Ld", v)
  | [@implicit_arity] Read([@implicit_arity] Calculation(v, c), s) =>
    pf(ppf, "(%a.%a[%a])", pp, v, Size.pp, s, Arithmetic.pp(pp), c)
  | [@implicit_arity] Read(v, s) => pf(ppf, "(%a.%a)", pp, v, Size.pp, s)
  | [@implicit_arity] Calculation(v, c) =>
    pf(ppf, "%a[%a]", pp, v, Arithmetic.pp(pp), c);

open Sigs;

let process:
  type s fd error.
    (scheduler(s), syscall(fd, error, s), fd, t, int64) =>
    io(result(int64, error), s) =
  ({bind, return} as scheduler, syscall, fd, offset, abs_offset) => {
    let (>>=) = bind;
    let (>?=) = (x, f) =>
      x
      >>= (
        fun
        | Ok(x) => f(x)
        | Error(err) => return(Error(err))
      );
    /* XXX(dinosaure): we want to calculate at any steps the absolute offset
       instead to trust on [CUR] and we try to use only [SET] with [seek].
       However, in some case (eg. [offset use name]), we must lie about the
       /absolute offset/. */
    let rec go_offset =
      fun
      | [@implicit_arity] Read([@implicit_arity] Calculation(v, c), s) =>
        go_offset(v)
        >?= (
          abs_offset =>
            syscall.seek(fd, abs_offset, SET)
            >?= (
              () =>
                Size.read(scheduler, syscall, fd, s)
                >?= (abs_offset => go_calculation(abs_offset, c))
            )
        )
      | [@implicit_arity] Read(v, s) =>
        go_offset(v)
        >?= (
          abs_offset =>
            syscall.seek(fd, abs_offset, SET)
            >?= (() => Size.read(scheduler, syscall, fd, s))
        )
      | Relative(v) =>
        go_offset(v)
        >?= (
          rel_offset => (return <.> ok)(Int64.add(abs_offset, rel_offset))
        )
      | Absolute(v) => go_offset(v)
      | Value(v) => (return <.> ok)(v)
      | [@implicit_arity] Calculation(v, c) =>
        go_offset(v) >?= (abs_offset => go_calculation(abs_offset, c))
    and go_calculation = abs_offset =>
      fun
      | Arithmetic.Invert(c) =>
        go_calculation(abs_offset, c)
        >?= (abs_offset => (return <.> ok)(Int64.lognot(abs_offset)))
      | Arithmetic.Add(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.add(abs_offset, v)))
      | Arithmetic.Sub(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.sub(abs_offset, v)))
      | Arithmetic.Mul(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.mul(abs_offset, v)))
      | Arithmetic.Div(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.div(abs_offset, v)))
      | Arithmetic.Mod(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.rem(abs_offset, v)))
      | Arithmetic.Bitwise_and(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.logand(abs_offset, v)))
      | Arithmetic.Bitwise_xor(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.logxor(abs_offset, v)))
      | Arithmetic.Bitwise_or(v) =>
        go_offset(v) >?= (v => (return <.> ok)(Int64.logor(abs_offset, v)));

    go_offset(offset);
  };
