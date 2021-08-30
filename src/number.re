type t =
  | Int(int64)
  | Float(float);

let int64 = v => Int(v);

let float = v => Float(v);

let pp = ppf =>
  fun
  | Int(v) => Format.fprintf(ppf, "%Ld", v)
  | Float(v) => Format.fprintf(ppf, "%f", v);

let to_float =
  fun
  | Int(v) => Int64.to_float(v)
  | Float(v) => v;

/* TODO(dinosaure): check with 32bits/64bits platform. */
let to_int =
  fun
  | Int(v) => Int64.to_int(v)
  | Float(v) => Float.to_int(v);

let to_int64 =
  fun
  | Int(v) => v
  | Float(v) => Int64.of_float(v);

let to_int32 =
  fun
  | Int(v) => Int64.to_int32(v)
  | Float(v) => Int32.of_float(v);

let to_short =
  fun
  | Int(v) => Int64.to_int(v) land 0xffff
  | Float(v) => Float.to_int(v) land 0xffff;

let to_byte =
  fun
  | Int(v) => Char.chr(Int64.to_int(v) land 0xff)
  | Float(v) => Char.chr(Float.to_int(v) land 0xff);

let to_ptime =
  fun
  | Int(v) => Ptime.Span.of_int_s(Int64.to_int(v))
  | Float(v) =>
    switch (Ptime.Span.of_float_s(v)) {
    | Some(v) => v
    | None => Fmt.invalid_arg("Invalid POSIX time value: %f", v)
    };

let parse = s => {
  open Sub;
  let v = to_string(s);
  let lexbuf = Lexing.from_string(v);
  if (is_empty(s)) {
    Error(`Empty);
  } else {
    switch (Lexer.int_or_float(lexbuf)) {
    | Ok(`Int(literal)) =>
      /* XXX(dinosaure): [literal] can not be empty (see [lexer.mll]). */
      switch (literal.[String.length(literal) - 1]) {
      | '_' => Error(`Invalid_number(v))
      | _ =>
        let first = Lexing.lexeme_end(lexbuf);
        [@implicit_arity]
        Ok(Int(Int64.of_string(literal)), with_range(~first, s));
      }
    | Ok(`Float(literal)) =>
      let first = Lexing.lexeme_end(lexbuf);
      [@implicit_arity]
      Ok(Float(Float.of_string(literal)), with_range(~first, s));
    | Error(`Malformed) => Error(`Invalid_number(v))
    | exception _ => Error(`Invalid_number(v))
    };
  };
};
