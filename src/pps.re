let char_to_int: Fmt.Hmap.Key.key(char) = (
  Fmt.Hmap.Key.create(): Fmt.Hmap.Key.key(char)
);

let pp_char_to_int = (~padding=?, ~precision=?, ppf, v) =>
  Fmt.pp_int(~padding?, ~precision?, ppf, Char.code(v));

let int32_to_int: Fmt.Hmap.Key.key(int32) = (
  Fmt.Hmap.Key.create(): Fmt.Hmap.Key.key(int32)
);

let pp_int32_to_int = (~padding=?, ~precision=?, ppf, v) =>
  Fmt.pp_int32(~padding?, ~precision?, ppf, v);

let v =
  Fmt.Hmap.(
    empty
    |> add(char_to_int, pp_char_to_int)
    |> add(int32_to_int, pp_int32_to_int)
  );

let ropes_to_string: Fmt.Hmap.Key.key(Ropes.t) = (
  Fmt.Hmap.Key.create(): Fmt.Hmap.Key.key(Ropes.t)
);

let pp_ropes_to_string = (~padding=?, ppf, v) => {
  let (str, off, len) = Ropes.to_string(v);
  Fmt.pp_string(~padding?, ppf, String.sub(str, off, len));
};
