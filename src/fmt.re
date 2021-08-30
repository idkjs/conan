/* XXX(dinosaure): at the beginning, we used [Format.format] but we can
   replace a "%*" (like "%d") by an user-defined one (like "%a"). This
   module wants to provide our own [Format.format] type with this ability
   with [Hmap]. See [Atom]. */

type refl('a, 'b) =
  | Refl: refl('a, 'a);

type formatter = Format.formatter;

module Hmap = {
  module Tid = {
    type t(_) = ..;
  };

  module type Tid = {
    type t;

    type Tid.t(_) +=
      | Tid: Tid.t(t);
  };

  type tid('a) = (module Tid with type t = 'a);

  let tid = ((), type s) => {
    module M = {
      type t = s;

      type Tid.t(_) +=
        | Tid: Tid.t(t);
    };
    ((module M): (module Tid with type t = s));
  };

  let refl: type a b. (tid(a), tid(b)) => option(refl(a, b)) =
    (r, s) => {
      module A = (val r: Tid with type t = a);
      module B = (val s: Tid with type t = b);
      switch (A.Tid) {
      | B.Tid => Some(Refl)
      | _ => None
      };
    };

  module Key = {
    type key('a) = {
      uid: int,
      tid: tid('a),
    };

    let uid = {
      let id = ref(-1);
      () => {
        incr(id);
        id^;
      };
    };

    let create = () => {
      let uid = uid();
      let tid = tid();
      {uid, tid};
    };

    type t =
      | V(key('a)): t;

    let hide_type = k => V(k);

    let equal = (V(k0), V(k1)) =>
      (compare: (int, int) => int)(k0.uid, k1.uid) == 0;

    let compare = (V(k0), V(k1)) =>
      (compare: (int, int) => int)(k0.uid, k1.uid);
  };

  type key('a) = Key.key('a);

  module Map = Map.Make(Key);

  type binding =
    | B(
        key('a),
        (
          ~padding: [ | `Left(int) | `Right(int) | `Zero(int)]=?,
          ~precision: int=?,
          formatter,
          'a
        ) =>
        unit,
      )
      : binding;

  type t = Map.t(binding);

  let empty = Map.empty;

  let add = (k, v, t) => Map.add(V(k), [@implicit_arity] B(k, v), t);

  let find:
    type a.
      (key(a), t) =>
      option(
        (
          ~padding: [ | `Left(int) | `Right(int) | `Zero(int)]=?,
          ~precision: int=?,
          Format.formatter,
          a
        ) =>
        unit,
      ) =
    (k, t) =>
      switch (Map.find(Key.V(k), t)) {
      | [@implicit_arity] B(k', v) =>
        switch (refl(k.Key.tid, k'.Key.tid)) {
        | Some(Refl) => Some(v)
        | None => None
        }
      | exception Not_found => None
      };

  let get = (k, t) =>
    switch (find(k, t)) {
    | Some(pp) => pp
    | None => invalid_arg("Key not found")
    };
};

let invalid_arg = fmt => Format.kasprintf(invalid_arg, fmt);

type t('a) = (formatter, 'a) => unit;

type padding('ty, 'v) =
  | Nop: padding('v, 'v)
  | Lit([ | `Left | `Right | `Zero], int): padding('v, 'v)
  | Arg([ | `Left | `Right | `Zero]): padding(int => 'v, 'v);

type precision('ty, 'v) =
  | Nop: precision('v, 'v)
  | Lit(int): precision('v, 'v)
  | Arg: precision(int => 'v, 'v);

type order('ty, 'v) =
  | Byte(padding('u, 'v), precision('v, char => 'w)): order('u, 'w)
  | Short(padding('u, 'v), precision('v, int => 'w)): order('u, 'w)
  | Long(padding('u, 'v), precision('v, int32 => 'w)): order('u, 'w)
  | Quad(padding('u, 'v), precision('v, int64 => 'w)): order('u, 'w)
  | Float(padding('u, 'v), precision('v, float => 'w)): order('u, 'w)
  | String(padding('u, string => 'v)): order('u, 'v)
  | Const(t('a), 'a): order('v, 'v)
  | Noop: order('v, 'v)
  | Atom(padding('u, 'v), precision('v, 'a => 'w), Hmap.key('a))
    : order('u, 'w)
  | Param: order((t('a), 'a) => 'v, 'v)
  | Ignore: order('a => 'v, 'v)

and fmt('ty, 'v) =
  | []: fmt('v, 'v)
  | ::(order('ty, 'v), fmt('v, 'r)): fmt('ty, 'r);

type ty('ty, 'v) = tyrel('ty, 'v, 'ty, 'v)

and tyrel('ty0, 'v0, 'ty1, 'v1) =
  | Char(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(char => 'ty0, 'v0, char => 'ty1, 'v1)
  | Int(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(int => 'ty0, 'v0, int => 'ty1, 'v1)
  | Int32(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(int32 => 'ty0, 'v0, int32 => 'ty1, 'v1)
  | Int64(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(int64 => 'ty0, 'v0, int64 => 'ty1, 'v1)
  | Float(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(float => 'ty0, 'v0, float => 'ty1, 'v1)
  | String(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel(string => 'ty0, 'v0, string => 'ty1, 'v1)
  | Param(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel((t('a), 'a) => 'ty0, 'v0, (t('a), 'a) => 'ty1, 'v1)
  | Any(Hmap.key('a), tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel('a => 'ty0, 'v0, 'a => 'ty1, 'v1)
  | Ignore(tyrel('ty0, 'v0, 'ty1, 'v1))
    : tyrel('a => 'ty0, 'v0, 'a => 'ty1, 'v1)
  | End: tyrel('v0, 'v0, 'v1, 'v1);

type tw('v, 'r) =
  | T(fmt('u, 'v), ty('v, 'w)): tw('u, 'w);

type pw('v, 'r) =
  | P(padding('u, 'v), ty('v, 'w)): pw('u, 'w);

type ppw('v, 'r) =
  | V(padding('a, 'b), precision('b, 'c), ty('c, 'd)): ppw('a, 'd);

type v =
  | Fmt(fmt('ty, 'v)): v;

type w =
  | Ty(ty('ty, 'v)): w;

let pf = Format.fprintf;

let strf = Format.asprintf;

let padding_and_precision_to_padding:
  type u v w. (padding(u, v), precision(v, w)) => padding(u, w) =
  (padding, precision) =>
    switch (padding, precision) {
    | (Nop, Nop) => Nop
    | (Nop, Lit(n)) => [@implicit_arity] Lit(`Left, n)
    | (Nop, Arg) => Arg(`Left)
    | ([@implicit_arity] Lit(dir, n), Nop) => [@implicit_arity] Lit(dir, n)
    | (Arg(dir), Nop) => Arg(dir)
    | (Arg(_), Arg) => assert(false) /* [int -> int -> *] can not be reduced to [int -> *] */
    | (Arg(dir), Lit(_)) => Arg(dir)
    | ([@implicit_arity] Lit(dir, _), Arg) => Arg(dir)
    | ([@implicit_arity] Lit(dir, n), Lit(_)) =>
      [@implicit_arity] Lit(dir, n)
    };

let pp_ty: type v r. (Format.formatter, ty(v, r)) => unit =
  (ppf, ty) => {
    let rec flat: type v r. (list(_), ty(v, r)) => list(_) =
      acc =>
        fun
        | End => List.rev(acc)
        | Char(ty) => flat([`Char, ...acc], ty)
        | Int(ty) => flat([`Int, ...acc], ty)
        | Int32(ty) => flat([`Int32, ...acc], ty)
        | Int64(ty) => flat([`Int64, ...acc], ty)
        | Float(ty) => flat([`Float, ...acc], ty)
        | String(ty) => flat([`String, ...acc], ty)
        | Param(ty) => flat([`Param, ...acc], ty)
        | [@implicit_arity] Any(_, ty) => flat([`Any, ...acc], ty)
        | Ignore(ty) => flat([`Ignore, ...acc], ty);
    let pp_ty = ppf =>
      fun
      | `Char => pf(ppf, "char")
      | `Int => pf(ppf, "int")
      | `Int32 => pf(ppf, "int32")
      | `Int64 => pf(ppf, "int64")
      | `Float => pf(ppf, "float")
      | `String => pf(ppf, "string")
      | `Param => pf(ppf, "'a t -> 'a")
      | `Any => pf(ppf, "'any")
      | `Ignore => pf(ppf, "'ignore");
    let pp_list = (pp_val, ~sep as pp_sep, ppf, lst) => {
      let rec go: list(_) => unit = (
        fun
        | [] => ()
        | [x] => pp_val(ppf, x)
        | [x, ...r] => {
            pf(ppf, "%a%a", pp_val, x, pp_sep, ());
            go(r);
          }:
          list(_) => unit
      );
      go(lst);
    };
    let sep = (ppf, ()) => pf(ppf, " -> ");
    pf(ppf, "(%a)", pp_list(~sep, pp_ty), flat([], ty));
  };

let ty_of_padding: type v r. padding(v, r) => ty(v, r) =
  fun
  | Nop => End
  | Lit(_) => End
  | Arg(_) => Int(End);

let ty_of_precision: type v r. precision(v, r) => ty(v, r) =
  fun
  | Nop => End
  | Lit(_) => End
  | Arg => Int(End);

let rec concat_ty: type u v w. (ty(u, v), ty(v, w)) => ty(u, w) =
  (l1, l2) =>
    switch (l1) {
    | End => l2
    | Char(l1) => Char(concat_ty(l1, l2))
    | Int(l1) => Int(concat_ty(l1, l2))
    | Int32(l1) => Int32(concat_ty(l1, l2))
    | Int64(l1) => Int64(concat_ty(l1, l2))
    | Float(l1) => Float(concat_ty(l1, l2))
    | String(l1) => String(concat_ty(l1, l2))
    | Param(l1) => Param(concat_ty(l1, l2))
    | [@implicit_arity] Any(key, l1) =>
      [@implicit_arity] Any(key, concat_ty(l1, l2))
    | Ignore(l1) => Ignore(concat_ty(l1, l2))
    };

let (@) = concat_ty;

let rec ty_of_fmt: type v r. fmt(v, r) => ty(v, r) =
  fun
  | [] => End
  | [Const(_), ...fmt] => ty_of_fmt(fmt)
  | [Noop, ...fmt] => ty_of_fmt(fmt)
  | [Ignore, ...fmt] => Ignore(ty_of_fmt(fmt))
  | [[@implicit_arity] Atom(padding, precision, key), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ [@implicit_arity] Any(key, ty_of_fmt(fmt));
    }
  | [Param, ...fmt] => Param(ty_of_fmt(fmt))
  | [[@implicit_arity] Byte(padding, precision), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ Char(ty_of_fmt(fmt));
    }
  | [[@implicit_arity] Short(padding, precision), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ Int(ty_of_fmt(fmt));
    }
  | [[@implicit_arity] Long(padding, precision), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ Int32(ty_of_fmt(fmt));
    }
  | [[@implicit_arity] Quad(padding, precision), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ Int64(ty_of_fmt(fmt));
    }
  | [[@implicit_arity] Float(padding, precision), ...fmt] => {
      let padding = ty_of_padding(padding);
      let precision = ty_of_precision(precision);
      padding @ precision @ Float(ty_of_fmt(fmt));
    }
  | [String(padding), ...fmt] => {
      let padding = ty_of_padding(padding);
      padding @ String(ty_of_fmt(fmt));
    };

exception Invalid_type;

exception Invalid_key;

let gen_padding:
  type ty0 v0 ty1 v1. (padding(ty0, v0), ty(ty1, v1)) => pw(ty1, v1) =
  (pad, ty) =>
    switch (pad, ty) {
    | (Nop, _) => [@implicit_arity] P(Nop, ty)
    | ([@implicit_arity] Lit(padding, pad), _) =>
      [@implicit_arity] P([@implicit_arity] Lit(padding, pad), ty)
    | (Arg(padding), Int(ty)) => [@implicit_arity] P(Arg(padding), ty)
    | _ => raise(Invalid_type)
    };

let gen_padding_precision:
  type u v w ty0 v0.
    (padding(u, v), precision(v, w), ty(ty0, v0)) => ppw(ty0, v0) =
  (padding, precision, ty) =>
    switch (precision, gen_padding(padding, ty)) {
    | (Nop, [@implicit_arity] P(padding, ty)) =>
      [@implicit_arity] V(padding, Nop, ty)
    | (Lit(v), [@implicit_arity] P(padding, ty)) =>
      [@implicit_arity] V(padding, Lit(v), ty)
    | (Arg, [@implicit_arity] P(padding, Int(ty))) =>
      [@implicit_arity] V(padding, Arg, ty)
    | _ => raise(Invalid_type)
    };

let rec gen: type ty0 v0 ty1 v1. (fmt(ty0, v0), ty(ty1, v1)) => tw(ty1, v1) =
  (fmt, ty) =>
    switch (fmt, ty) {
    | ([], rest) => [@implicit_arity] T([], rest)
    | ([String(padding), ...fmt_rest], _) =>
      switch (gen_padding(padding, ty)) {
      | [@implicit_arity] P(padding, String(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity] T([String(padding), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Const(pp, v), ...fmt_rest], ty_rest) =>
      let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
      [@implicit_arity] T([[@implicit_arity] Const(pp, v), ...fmt], ty);
    | ([[@implicit_arity] Byte(padding, precision), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity] V(padding, precision, Char(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity]
        T([[@implicit_arity] Byte(padding, precision), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Short(padding, precision), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity] V(padding, precision, Int(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity]
        T([[@implicit_arity] Short(padding, precision), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Long(padding, precision), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity] V(padding, precision, Int32(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity]
        T([[@implicit_arity] Long(padding, precision), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Quad(padding, precision), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity] V(padding, precision, Int64(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity]
        T([[@implicit_arity] Quad(padding, precision), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Float(padding, precision), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity] V(padding, precision, Float(ty_rest)) =>
        let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
        [@implicit_arity]
        T([[@implicit_arity] Float(padding, precision), ...fmt], ty);
      | _ => raise(Invalid_type)
      }
    | ([[@implicit_arity] Atom(padding, precision, key0), ...fmt_rest], _) =>
      switch (gen_padding_precision(padding, precision, ty)) {
      | [@implicit_arity]
        V(padding, precision, [@implicit_arity] Any(key1, ty_rest)) =>
        if (!Hmap.Key.(equal(hide_type(key0), hide_type(key1)))) {
          raise(Invalid_key);
        } else {
          switch (Hmap.refl(key0.Hmap.Key.tid, key1.Hmap.Key.tid)) {
          | Some(Refl) =>
            let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
            [@implicit_arity]
            T(
              [[@implicit_arity] Atom(padding, precision, key0), ...fmt],
              ty,
            );
          | None => raise(Invalid_key)
          };
        }
      | _ => raise(Invalid_type)
      }
    | ([Noop, ...fmt_rest], ty_rest) => gen(fmt_rest, ty_rest)
    | ([Ignore, ...fmt_rest], Ignore(ty_rest)) =>
      let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
      [@implicit_arity] T([Ignore, ...fmt], ty);
    | ([Param, ...fmt_rest], Param(ty_rest)) =>
      let [@implicit_arity] T(fmt, ty) = gen(fmt_rest, ty_rest);
      [@implicit_arity] T([Param, ...fmt], ty);
    | ([Param, ..._], _) => raise(Invalid_type)
    | ([Ignore, ..._], _) => raise(Invalid_type)
    };

let rec concat: type u v w. (fmt(u, v), fmt(v, w)) => fmt(u, w) =
  (l1, l2) =>
    switch (l1) {
    | [] => l2
    | [x, ...r] => [x, ...concat(r, l2)]
    };

/* XXX(dinosaure): tail-rec? */

let (^^) = concat;

let pp_char = (~padding=?, ~precision=?, ppf, v) =>
  switch (padding, precision) {
  | (Some(`Left(padding)), _) =>
    pf(ppf, Scanf.format_from_string(strf("%%-%dc", padding), "%c"), v)
  | (Some(`Zero(_)), _) => pf(ppf, "%0c", v)
  | _ => pf(ppf, "%c", v)
  };

let pp_int = (~padding=?, ~precision=?, ppf, v) =>
  switch (padding, precision) {
  | (None, None) => pf(ppf, "%d", v)
  | (Some(`Left(padding)), None) => pf(ppf, "%-*d", padding, v)
  | (Some(`Zero(padding)), _) => pf(ppf, "%0*d", padding, v)
  | (Some(`Right(padding)), None) => pf(ppf, "%*d", padding, v)
  | (None, Some(precision)) => pf(ppf, "%.*d", precision, v)
  | (Some(`Left(padding)), Some(precision)) =>
    pf(ppf, "%-*.*d", padding, precision, v)
  | (Some(`Right(padding)), Some(precision)) =>
    pf(ppf, "%*.*d", padding, precision, v)
  };

let pp_int32 = (~padding=?, ~precision=?, ppf, v) =>
  switch (padding, precision) {
  | (None, None) => pf(ppf, "%ld", v)
  | (Some(`Left(padding)), None) => pf(ppf, "%-*ld", padding, v)
  | (Some(`Zero(padding)), _) => pf(ppf, "%0*ld", padding, v)
  | (Some(`Right(padding)), None) => pf(ppf, "%*ld", padding, v)
  | (None, Some(precision)) => pf(ppf, "%.*ld", precision, v)
  | (Some(`Left(padding)), Some(precision)) =>
    pf(ppf, "%-*.*ld", padding, precision, v)
  | (Some(`Right(padding)), Some(precision)) =>
    pf(ppf, "%*.*ld", padding, precision, v)
  };

let pp_int64 = (~padding=?, ~precision=?, ppf, v) =>
  switch (padding, precision) {
  | (None, None) => pf(ppf, "%Ld", v)
  | (Some(`Left(padding)), None) => pf(ppf, "%-*Ld", padding, v)
  | (Some(`Zero(padding)), _) => pf(ppf, "%0*Ld", padding, v)
  | (Some(`Right(padding)), None) => pf(ppf, "%*Ld", padding, v)
  | (None, Some(precision)) => pf(ppf, "%.*Ld", precision, v)
  | (Some(`Left(padding)), Some(precision)) =>
    pf(ppf, "%-*.*Ld", padding, precision, v)
  | (Some(`Right(padding)), Some(precision)) =>
    pf(ppf, "%*.*Ld", padding, precision, v)
  };

let pp_float = (~padding=?, ~precision=?, ppf, v) =>
  switch (padding, precision) {
  | (None, None) => pf(ppf, "%f", v)
  | (Some(`Left(padding)), None) => pf(ppf, "%-*f", padding, v)
  | (Some(`Zero(padding)), _) => pf(ppf, "%0*f", padding, v)
  | (Some(`Right(padding)), None) => pf(ppf, "%*f", padding, v)
  | (None, Some(precision)) => pf(ppf, "%.*f", precision, v)
  | (Some(`Left(padding)), Some(precision)) =>
    pf(ppf, "%-*.*f", padding, precision, v)
  | (Some(`Right(padding)), Some(precision)) =>
    pf(ppf, "%*.*f", padding, precision, v)
  };

let pp_string = (~padding=?, ppf, v) =>
  switch (padding) {
  | Some(`Left(padding)) => pf(ppf, "%-*s", padding, v)
  | Some(`Right(padding)) => pf(ppf, "%*s", padding, v)
  | _ => pf(ppf, "%s", v)
  };

type wpd =
  | Pd(padding('v, 'r)): wpd;

type wpr =
  | Pr(precision('v, 'r)): wpr;

let is_flag =
  fun
  | '-'
  | '0' => true
  | _ => false;

let is_dash =
  fun
  | '-' => true
  | _ => false;

let is_zero =
  fun
  | '0' => true
  | _ => false;

let is_digit =
  fun
  | '0' .. '9' => true
  | _ => false;

type s = Sub.t;

type ebb('r) =
  | Ebb(fmt('x, 'r)): ebb('r);

type pdebb('x, 'r) =
  | PdEbb(padding(_, 'x => 'a), fmt('a, 'r)): pdebb('x, 'r);

type prebb('x, 'r) =
  | PrEbb(precision(_, 'x => 'a), fmt('a, 'r)): prebb('x, 'r);

type pdprebb('x, 'r) =
  | PdPrEbb(padding('u, 'v), precision('v, 'w => 'a), fmt('a, 'r))
    : pdprebb('w, 'r);

let make_prebb: type a b. (precision(a, b), fmt(_, _)) => prebb(_, _) =
  (precision, fmt) =>
    switch (precision) {
    | Nop => [@implicit_arity] PrEbb(Nop, fmt)
    | Lit(v) => [@implicit_arity] PrEbb(Lit(v), fmt)
    | Arg => [@implicit_arity] PrEbb(Arg, fmt)
    };

let make_pdebb: type a b. (padding(a, b), fmt(_, _)) => pdebb(_, _) =
  (padding, fmt) =>
    switch (padding) {
    | Nop => [@implicit_arity] PdEbb(Nop, fmt)
    | [@implicit_arity] Lit(k, v) =>
      [@implicit_arity] PdEbb([@implicit_arity] Lit(k, v), fmt)
    | Arg(v) => [@implicit_arity] PdEbb(Arg(v), fmt)
    };

let make_pdprebb:
  type a b c d.
    (padding(a, b), precision(c, d), fmt(_, _)) => pdprebb(_, _) =
  (padding, precision, fmt) => {
    let [@implicit_arity] PrEbb(precision, fmt) = make_prebb(precision, fmt);
    switch (padding) {
    | Nop => [@implicit_arity] PdPrEbb(Nop, precision, fmt)
    | [@implicit_arity] Lit(k, v) =>
      [@implicit_arity] PdPrEbb([@implicit_arity] Lit(k, v), precision, fmt)
    | Arg(v) => [@implicit_arity] PdPrEbb(Arg(v), precision, fmt)
    };
  };

let rec parse_precision:
  type x v r. (~any: Hmap.key(x), padding(v, _), s) => ebb(r) =
  (~any, padding, s) => {
    open Sub;
    let dot = v(".");
    if (is_prefix(~affix=dot, s)) {
      let (precision, s) = span(~sat=is_digit, tail(s));
      switch (to_string(precision)) {
      | "" => parse_flag(~any, padding, Arg, s)
      | precision =>
        parse_flag(padding, ~any, Lit(int_of_string(precision)), s)
      };
    } else {
      parse_flag(~any, padding, Nop, s);
    };
  }

and parse_padding: type x r. (~any: Hmap.key(x), s) => ebb(r) =
  (~any, s) => {
    open Sub;
    let (flags, s) = span(~sat=is_flag, s);
    let left_padding = exists(is_dash, flags);
    let zero_padding = exists(is_zero, flags);
    let (padding, s) = span(~sat=is_digit, s);
    switch (left_padding, zero_padding, to_string(padding)) {
    | (false, false, "") => parse_precision(~any, Nop, s)
    | (true, false, "") =>
      parse_precision(~any, [@implicit_arity] Lit(`Right, 0), s)
    /* XXX(dinosaure): check "%-s" please! I don't know what it should do! */
    | (false, true, "") => parse_precision(~any, Arg(`Zero), s)
    | (false, false, pad) =>
      parse_precision(
        ~any,
        [@implicit_arity] Lit(`Right, int_of_string(pad)),
        s,
      )
    | (true, false, pad) =>
      parse_precision(
        ~any,
        [@implicit_arity] Lit(`Left, int_of_string(pad)),
        s,
      )
    | (false, true, pad) =>
      parse_precision(
        ~any,
        [@implicit_arity] Lit(`Zero, int_of_string(pad)),
        s,
      )
    | (true, true, _) => invalid_arg("Invalid padding flags")
    };
  }

and parse_flag:
  type x a b c d r.
    (~any: Hmap.key(x), padding(a, b), precision(c, d), s) => ebb(r) =
  (~any, padding, precision, s) =>
    Sub.(
      switch (head(s)) {
      | None => invalid_arg("Invalid format: %S", to_string(s))
      | Some('%') =>
        let Ebb(fmt) = go(~any, tail(s));
        Ebb([[@implicit_arity] Const(pp_string, "%"), ...fmt]);
      | Some('a') =>
        let Ebb(fmt) = go(~any, tail(s));
        Ebb([Param, ...fmt]);
      | Some('c') =>
        let Ebb(fmt) = go(~any, tail(s));
        let [@implicit_arity] PdPrEbb(padding, precision, fmt) =
          make_pdprebb(padding, precision, fmt);
        Ebb([[@implicit_arity] Byte(padding, precision), ...fmt]);
      | Some('d') =>
        let Ebb(fmt) = go(~any, tail(s));
        let [@implicit_arity] PdPrEbb(padding, precision, fmt) =
          make_pdprebb(padding, precision, fmt);
        Ebb([[@implicit_arity] Short(padding, precision), ...fmt]);
      | Some('!') =>
        let Ebb(fmt) = go(~any, tail(s));
        let [@implicit_arity] PdPrEbb(padding, precision, fmt) =
          make_pdprebb(padding, precision, fmt);
        Ebb([[@implicit_arity] Atom(padding, precision, any), ...fmt]);
      | Some('s') =>
        let Ebb(fmt) = go(~any, tail(s));
        let [@implicit_arity] PdPrEbb(padding, precision, fmt) =
          make_pdprebb(padding, precision, fmt);
        let padding = padding_and_precision_to_padding(padding, precision);
        Ebb([String(padding), ...fmt]);
      | Some(chr) => invalid_arg("Invalid formatter %c", chr)
      }
    )

and go: type x r. (~any: Hmap.key(x), s) => ebb(r) =
  (~any, s) => {
    open Sub;
    let percent = v("%");
    switch (cut(~sep=percent, s)) {
    | None =>
      if (is_empty(s)) {
        Ebb([]);
      } else {
        Ebb([[@implicit_arity] Const(pp_string, to_string(s))]);
      }
    | Some((x, s)) =>
      if (!is_empty(x)) {
        let Ebb(fmt) = parse_padding(~any, s);
        Ebb([[@implicit_arity] Const(pp_string, to_string(x)), ...fmt]);
      } else {
        parse_padding(~any, s);
      }
    };
  }

and parse = (~any, fmt) => Sub.(go(~any, v(fmt)));

let coerce: type v0 r0 v1 r1. (fmt(v0, r0), ty(v1, r1)) => fmt(v1, r1) =
  (fmt, ty) =>
    switch (gen(fmt, ty)) {
    | [@implicit_arity] T(fmt, End) => fmt
    | _ => invalid_arg("coerce")
    };

let of_string:
  type x v r. (~any: Hmap.key(x), string, ty(v, r)) => fmt(v, r) =
  (~any, s, ty) => {
    let Ebb(fmt) = parse(~any, s);
    coerce(fmt, ty);
  };

let ty_of_string: type x. (~any: Hmap.key(x), string) => w =
  (~any, s) => {
    let Ebb(fmt) = parse(~any, s);
    Ty(ty_of_fmt(fmt));
  };

let noop: order(_) = (Noop: order(_));

let ignore: order(_) = (Ignore: order(_));

let const = (pp, v) => [@implicit_arity] Const(pp, v);

let ($) = const;

let padding =
  fun
  | `Left => (x => `Left(x))
  | `Right => (x => `Right(x))
  | `Zero => (x => `Zero(x));

let keval_padding:
  type ty v.
    (
      formatter,
      padding(ty, v),
      (formatter, option([ | `Left(int) | `Right(int) | `Zero(int)])) => v
    ) =>
    ty =
  (ppf, padding, k) =>
    switch (padding) {
    | Nop => k(ppf, None)
    | [@implicit_arity] Lit(`Left, v) => k(ppf, Some(`Left(v)))
    | [@implicit_arity] Lit(`Right, v) => k(ppf, Some(`Right(v)))
    | [@implicit_arity] Lit(`Zero, v) => k(ppf, Some(`Zero(v)))
    | Arg(`Left) => (v => k(ppf, Some(`Left(v))))
    | Arg(`Right) => (v => k(ppf, Some(`Right(v))))
    | Arg(`Zero) => (v => k(ppf, Some(`Zero(v))))
    };

let keval_precision:
  type ty v.
    (formatter, precision(ty, v), (formatter, option(int)) => v) => ty =
  (ppf, precision, k) =>
    switch (precision) {
    | Nop => k(ppf, None)
    | Lit(v) => k(ppf, Some(v))
    | Arg => (v => k(ppf, Some(v)))
    };

let keval_order:
  type ty v. (Hmap.t, formatter, order(ty, v), formatter => v) => ty =
  (pps, ppf, order, k) =>
    switch (order) {
    | Ignore => (_ => k(ppf))
    | Noop => k(ppf)
    | [@implicit_arity] Const(pp, v) =>
      pp(ppf, v);
      k(ppf);
    | Param => (
        (pp, v) => {
          pp(ppf, v);
          k(ppf);
        }
      )
    | [@implicit_arity] Atom(padding, precision, key) =>
      let k = (padding, ppf, precision, v) => {
        let pp = Hmap.get(key, pps);
        pp(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    | String(padding) =>
      let k = (ppf, padding, v) => {
        pp_string(~padding?, ppf, v);
        k(ppf);
      };
      keval_padding(ppf, padding, k);
    | [@implicit_arity] Byte(padding, precision) =>
      let k = (padding, ppf, precision, v) => {
        pp_char(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    | [@implicit_arity] Short(padding, precision) =>
      let k = (padding, ppf, precision, v) => {
        pp_int(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    | [@implicit_arity] Long(padding, precision) =>
      let k = (padding, ppf, precision, v) => {
        pp_int32(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    | [@implicit_arity] Quad(padding, precision) =>
      let k = (padding, ppf, precision, v) => {
        pp_int64(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    | [@implicit_arity] Float(padding, precision) =>
      let k = (padding, ppf, precision, v) => {
        pp_float(~padding?, ~precision?, ppf, v);
        k(ppf);
      };
      let k = (ppf, padding) => keval_precision(ppf, precision, k(padding));
      keval_padding(ppf, padding, k);
    };

let rec keval:
  type ty v. (Hmap.t, formatter, fmt(ty, v), formatter => v) => ty =
  (pps, formatter, fmt, k) =>
    switch (fmt) {
    | [] => k(formatter)
    | [x, ...r] =>
      let k' = formatter => keval(pps, formatter, r, k);
      keval_order(pps, formatter, x, k');
    };
