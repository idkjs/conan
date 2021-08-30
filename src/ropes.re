type t =
  | Str(string, int, int)
  | App(t, t, int, int);

let empty = "";

let empty = [@implicit_arity] Str(empty, 0, 0);

let of_string = (~off=0, ~len=?, str) => {
  let len =
    switch (len) {
    | Some(len) => len
    | None => String.length(str) - off
    };
  [@implicit_arity] Str(str, off, len);
};

let length =
  fun
  | [@implicit_arity] Str(_, _, len) => len
  | [@implicit_arity] App(_, _, len, _) => len;

let height =
  fun
  | Str(_) => 0
  | [@implicit_arity] App(_, _, _, height) => height;

let app =
  fun
  | ([@implicit_arity] Str(_, _, 0), t)
  | (t, [@implicit_arity] Str(_, _, 0)) => t
  | (
      [@implicit_arity] Str(s0, off0, len0),
      [@implicit_arity] Str(s1, off1, len1),
    )
      when len0 + len1 < 256 => {
      let s = Bytes.create(len0 + len1);
      Bytes.blit_string(s0, off0, s, 0, len0);
      Bytes.blit_string(s1, off1, s, len0, len1);
      [@implicit_arity] Str(Bytes.unsafe_to_string(s), 0, len0 + len1);
    }
  | (
      [@implicit_arity]
      App(t0, [@implicit_arity] Str(s1, off1, len1), _len0, _height0),
      [@implicit_arity] Str(s2, off2, len2),
    )
      when len1 + len2 < 256 => {
      let s = Bytes.create(len1 + len2);
      Bytes.blit_string(s1, off1, s, 0, len1);
      Bytes.blit_string(s2, off2, s, len1, len2);
      [@implicit_arity]
      App(
        t0,
        [@implicit_arity] Str(Bytes.unsafe_to_string(s), 0, len1 + len2),
        length(t0) + len1 + len2,
        1 + height(t0),
      );
    }
  | (
      [@implicit_arity] Str(s0, off0, len0),
      [@implicit_arity]
      App([@implicit_arity] Str(s1, off1, len1), t2, _len2, _height2),
    )
      when len0 + len1 < 256 => {
      let s = Bytes.create(len0 + len1);
      Bytes.blit_string(s0, off0, s, 0, len0);
      Bytes.blit_string(s1, off1, s, len0, len1);
      [@implicit_arity]
      App(
        [@implicit_arity] Str(Bytes.unsafe_to_string(s), 0, len0 + len1),
        t2,
        len0 + len1 + length(t2),
        1 + height(t2),
      );
    }
  | (t0, t1) =>
    [@implicit_arity]
    App(t0, t1, length(t0) + length(t1), max(height(t0), height(t1)));

let append = (a, b) => app((a, b));

let concat = (~sep as str, lst) => {
  let sep = [@implicit_arity] Str(str, 0, String.length(str));
  let rec go = t =>
    fun
    | [] => t
    | [x] => append(t, x)
    | [x, ...r] => go(append(x, sep), r);
  go(empty, lst);
};

let rec unsafe_blit = (t, buf, off, len) =>
  switch (t) {
  | [@implicit_arity] Str(str0, off0, len0) =>
    let len' = min(len0, len);
    Bytes.blit_string(str0, off0, buf, off, len');
  | [@implicit_arity] App([@implicit_arity] Str(str0, off0, len0), t1, _, _) =>
    let len' = min(len0, len);
    Bytes.blit_string(str0, off0, buf, off, len');
    unsafe_blit(t1, buf, off + len', len - len');
  | [@implicit_arity] App(t0, t1, _, _) =>
    let len' = min(length(t0), len);
    unsafe_blit(t0, buf, off, len');
    unsafe_blit(t1, buf, off + len', len - len');
  };

let to_string =
  fun
  | [@implicit_arity] Str(s, off, len) => (s, off, len)
  | [@implicit_arity] App(t0, t1, len, _) => {
      let res = Bytes.create(len);
      let l0 = length(t0);
      let l1 = length(t1);
      unsafe_blit(t0, res, 0, l1);
      unsafe_blit(t1, res, l0, l1);
      (Bytes.unsafe_to_string(res), 0, len);
    };
