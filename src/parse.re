/* XXX(dinosaure): anyone can say that this parser is a sh*t and he is true. */

type s = Sub.t;

let string_head = s =>
  if (String.length(s) > 0) {
    Some(s.[0]);
  } else {
    None;
  };

let string_tail = s =>
  if (String.length(s) > 0) {
    String.sub(s, 1, String.length(s) - 1);
  } else {
    "";
  };

let (>>=) = (x, f) =>
  switch (x) {
  | Ok(x) => f(x)
  | Error(err) => Error(err)
  };

let (>|=) = (x, f) =>
  switch (x) {
  | Ok(x) => Ok(f(x))
  | Error(err) => Error(err)
  };

let (<.>) = (f, g, x) => f(g(x));

open Sub;

let is_wsp =
  fun
  | ' '
  | '\t' .. '\r' => true
  | _ => false;

let is_digit =
  fun
  | '0' .. '9' => true
  | _ => false;

let is_greater =
  fun
  | '>' => true
  | _ => false;

let is_ampersand =
  fun
  | '&' => true
  | _ => false;

let is_c =
  fun
  | 'c' => true
  | _ => false;

let is_C =
  fun
  | 'C' => true
  | _ => false;

let is_b =
  fun
  | 'b' => true
  | _ => false;

let is_B =
  fun
  | 'B' => true
  | _ => false;

let is_s =
  fun
  | 's' => true
  | _ => false;

let is_r =
  fun
  | 'r' => true
  | _ => false;

let is_l =
  fun
  | 'l' => true
  | _ => false;

let is_t =
  fun
  | 't' => true
  | _ => false;

let is_T =
  fun
  | 'T' => true
  | _ => false;

let is_w =
  fun
  | 'w' => true
  | _ => false;

let is_W =
  fun
  | 'W' => true
  | _ => false;

let lparent = v("(")

and rparent = v(")");

let ampersand = v("&");

let tilde = v("~");

let dot = v(".");

let u = v("u");

let slash = v("/");

let parse_disp = s =>
  if (is_prefix(~affix=lparent, s) && is_suffix(~affix=rparent, s)) {
    Integer.parse((tail <.> tail(~rev=true))(s))
    >|= (((v, s)) => (`Ind(v), s));
  } else {
    Integer.parse(s) >|= (((v, s)) => (`Dir(v), s));
  };

let parse_abs_or_rel = s => {
  let (rel, s) =
    if (is_prefix(~affix=ampersand, s)) {
      (true, tail(s));
    } else {
      (false, s);
    };
  Integer.parse(s)
  >|= (
    ((v, s)) =>
      if (rel) {
        (`Rel(v), s);
      } else {
        (`Abs(v), s);
      }
  );
};

let parse_indirect_offset = s =>
  if (is_prefix(~affix=lparent, s) && is_suffix(~affix=rparent, s)) {
    let s = (tail <.> tail(~rev=true))(s);
    parse_abs_or_rel(s)
    >>= (
      ((offset, s)) => {
        let (size, s) =
          if (is_prefix(~affix=dot, s)) {
            let (size, s) = (span(~max=1, ~sat=Size.is_size) <.> tail)(s);
            (Some((Size.of_string <.> to_string)(size)), s);
          } else {
            (None, s);
          };
        let (arithmetic, s) = {
          let (invert, s) =
            if (is_prefix(~affix=tilde, s)) {
              (true, tail(s));
            } else {
              (false, s);
            };
          let (arithmetic, s) = span(~min=1, ~max=1, ~sat=Arithmetic.is, s);
          if (length(arithmetic) == 1) {
            (Some((invert, to_string(arithmetic))), s);
          } else {
            (None, s);
          };
        };
        switch (arithmetic) {
        | Some((invert, arithmetic)) =>
          parse_disp(s)
          >>= (
            ((disp, empty)) =>
              if (is_empty(empty)) {
                let arithmetic =
                  Arithmetic.of_string(~with_val=disp, arithmetic);
                let arithmetic =
                  if (invert) {
                    Arithmetic.invert(arithmetic);
                  } else {
                    arithmetic;
                  };
                [@implicit_arity] Ok(offset, size, Some(arithmetic));
              } else {
                Error(`Unexpected_trailer(empty));
              }
          )
        | None => [@implicit_arity] Ok(offset, size, None)
        };
      }
    );
  } else {
    Error(`Unmatched_parenthesis(s));
  };

let parse_offset = s => {
  let (level, s) = span(~sat=is_greater, s);
  let (rel, ss) = span(~sat=is_ampersand, s);
  if (is_prefix(~affix=lparent, ss)) {
    parse_indirect_offset(ss)
    >|= (
      v => (
        length(level),
        `Ind((
          if (!is_empty(rel)) {
            `Rel;
          } else {
            `Abs;
          },
          v,
        )),
      )
    );
  } else {
    parse_abs_or_rel(s)
    >>= (
      ((offset, empty)) =>
        if (is_empty(empty)) {
          [@implicit_arity] Ok(length(level), offset);
        } else {
          Error(`Unexpected_trailer(s));
        }
    );
  };
};

let prefix = (~affix, s) =>
  switch (cut(~sep=affix, s)) {
  | Some((empty, s)) =>
    if (is_empty(empty)) {
      Ok(s);
    } else {
      Error(`No_prefix((affix, s)));
    }
  | None => Error(`No_prefix((affix, s)))
  };

let le = v("le")

and be = v("be")

and me = v("me");

let byte = v("byte")

and long = v("long")

and quad = v("quad")

and date = v("date")

and clear = v("clear")

and short = v("short")

and ldate = v("ldate")

and regex = v("regex")

and qdate = v("qdate")

and float = v("float")

and double = v("double")

and qldate = v("qldate")

and qwdate = v("qwdate")

and search = v("search")

and string = v("string")

and pstring = v("pstring")

and default = v("default")

and indirect = v("indirect");

let _16 = v("16");

type numeric = [
  | `Byte
  | `Short
  | `Long
  | `Quad
  | `Date
  | `Ldate
  | `Qdate
  | `Qldate
  | `Qwdate
  | `Double
  | `Float
];

type default = [ | `Default];

type regex = [ | `Regex];

let parse_type = s => {
  let (unsigned, s) =
    if (is_prefix(~affix=u, s)) {
      (true, tail(s));
    } else {
      (false, s);
    };
  let is_le = is_prefix(~affix=le, s);
  let is_be = is_prefix(~affix=be, s);
  let is_me = is_prefix(~affix=me, s);
  let s =
    if (is_le || is_be || is_me) {
      (tail <.> tail)(s);
    } else {
      s;
    };
  let res =
    switch (String.sub(to_string(s), 0, 4)) {
    | "clea" => prefix(~affix=clear, s) >|= (s => (`Clear, s))
    | "byte" => prefix(~affix=byte, s) >|= (s => (`Byte, s))
    | "shor" => prefix(~affix=short, s) >|= (s => (`Short, s))
    | "long" => prefix(~affix=long, s) >|= (s => (`Long, s))
    | "quad" => prefix(~affix=quad, s) >|= (s => (`Quad, s))
    | "doub" => prefix(~affix=double, s) >|= (s => (`Double, s))
    | "floa" => prefix(~affix=float, s) >|= (s => (`Float, s))
    | "date" => prefix(~affix=date, s) >|= (s => (`Date, s))
    | "ldat" => prefix(~affix=ldate, s) >|= (s => (`Ldate, s))
    | "qdat" => prefix(~affix=qdate, s) >|= (s => (`Qdate, s))
    | "qlda" => prefix(~affix=qldate, s) >|= (s => (`Qldate, s))
    | "qwda" => prefix(~affix=qwdate, s) >|= (s => (`Qwdate, s))
    | "rege" => prefix(~affix=regex, s) >|= (s => (`Regex, s))
    | "sear" => prefix(~affix=search, s) >|= (s => (`Search, s))
    | "stri" =>
      prefix(~affix=string, s)
      >>= (
        s => {
          let string16 = is_prefix(~affix=_16, s);
          if (string16) {
            prefix(~affix=_16, s) >|= (s => (`String16, s));
          } else {
            [@implicit_arity] Ok(`String, s);
          };
        }
      )
    | "pstr" => prefix(~affix=pstring, s) >|= (s => (`Pstring, s))
    | "defa" => prefix(~affix=default, s) >|= (s => (`Default, s))
    | "indi" => prefix(~affix=indirect, s) >|= (s => (`Indirect, s))
    | _ => Error(`Invalid_type(s))
    | exception _ => Error(`Invalid_type(s))
    };
  let endian =
    switch (is_le, is_be, is_me) {
    | (true, false, false) => Some(`LE)
    | (false, true, false) => Some(`BE)
    | (false, false, true) => Some(`ME)
    | (false, false, false) => None
    | _ => assert(false)
    } /* XXX(dinosaure): should never occur! */;
  res
  >>= (
    ((kind, s)) =>
      switch (kind) {
      | `Clear => [@implicit_arity] Ok(unsigned, `Clear)
      | `Indirect =>
        switch (cut(~sep=slash, s)) {
        | None => [@implicit_arity] Ok(unsigned, `Indirect(false))
        | Some((empty, s)) =>
          if (is_empty(empty)) {
            let has_r = exists(is_r, s);
            [@implicit_arity] Ok(unsigned, `Indirect(has_r));
          } else {
            Error(`Unexpected_trailer(empty));
          }
        }
      | #default => [@implicit_arity] Ok(unsigned, `Default)
      | #regex =>
        switch (cut(~sep=slash, s)) {
        | None => [@implicit_arity] Ok(unsigned, `Regex(None))
        | Some((empty, s)) =>
          if (is_empty(empty)) {
            switch (cut(~sep=slash, s)) {
            | Some((a, b)) =>
              let (limit, flags) =
                if (for_all(is_digit, a)) {
                  (a, b);
                } else {
                  (b, a);
                };
              let has_c = exists(is_c, flags);
              let has_s = exists(is_s, flags);
              let has_l = exists(is_l, flags);
              Integer.parse(limit)
              >>= (
                ((limit, _empty)) =>
                  [@implicit_arity]
                  Ok(unsigned, `Regex(Some((has_c, has_s, has_l, limit))))
              );
            | None =>
              if (for_all(is_digit, s)) {
                Integer.parse(s)
                >>= (
                  ((limit, _empty)) =>
                    [@implicit_arity]
                    Ok(unsigned, `Regex(Some((false, false, false, limit))))
                );
              } else {
                let has_c = exists(is_c, s);
                let has_s = exists(is_s, s);
                let has_l = exists(is_l, s);
                [@implicit_arity]
                Ok(unsigned, `Regex(Some((has_c, has_s, has_l, 8192L))));
              }
            };
          } else {
            Error(`Unexpected_trailer(empty));
          }
        }
      | `String
      | `Search =>
        switch (cut(~sep=slash, s)) {
        | None => [@implicit_arity] Ok(unsigned, `Search(None))
        | Some((empty, s)) =>
          if (is_empty(empty)) {
            switch (cut(~sep=slash, s)) {
            | Some((a, b)) =>
              let (limit, flags) =
                switch (Integer.parse(a)) {
                | Ok(_) => (a, b)
                | Error(_) => (b, a)
                };

              let v = [];
              let v =
                if (exists(is_b, flags)) {
                  [`b, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_B, flags)) {
                  [`B, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_c, flags)) {
                  [`c, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_C, flags)) {
                  [`C, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_t, flags)) {
                  [`t, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_W, flags)) {
                  [`W, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_w, flags)) {
                  [`w, ...v];
                } else {
                  v;
                };
              let v =
                if (exists(is_T, flags)) {
                  [`T, ...v];
                } else {
                  v;
                };
              Integer.parse(limit)
              >>= (
                ((limit, empty)) =>
                  if (is_empty(empty)) {
                    [@implicit_arity]
                    Ok(unsigned, `Search(Some((v, Some(limit)))));
                  } else {
                    Error(`Unexpected_trailer(empty));
                  }
              );
            | None =>
              switch (Integer.parse(s)) {
              | [@implicit_arity] Ok(limit, empty) =>
                if (is_empty(empty)) {
                  [@implicit_arity]
                  Ok(unsigned, `Search(Some(([], Some(limit)))));
                } else {
                  Error(`Unexpected_trailer(empty));
                }
              | Error(`Empty | `Invalid_integer(_)) =>
                let v = [];
                let v =
                  if (exists(is_b, s)) {
                    [`b, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_B, s)) {
                    [`B, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_c, s)) {
                    [`c, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_C, s)) {
                    [`C, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_t, s)) {
                    [`t, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_W, s)) {
                    [`W, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_w, s)) {
                    [`w, ...v];
                  } else {
                    v;
                  };
                let v =
                  if (exists(is_T, s)) {
                    [`T, ...v];
                  } else {
                    v;
                  };
                [@implicit_arity] Ok(unsigned, `Search(Some((v, None))));
              }
            };
          } else {
            Error(`Unexpected_trailer(empty));
          }
        }
      | `Pstring =>
        switch (cut(~sep=slash, s)) {
        | None => [@implicit_arity] Ok(unsigned, `String8(None))
        | Some((empty, s)) =>
          if (is_empty(empty)) {
            let has_b = exists(is_b, s);
            let has_B = exists(is_B, s);
            let has_c = exists(is_c, s);
            let has_C = exists(is_C, s);
            [@implicit_arity]
            Ok(unsigned, `String8(Some((has_b, has_B, has_c, has_C))));
          } else {
            Error(`Unexpected_trailer(empty));
          }
        }
      | `String16 =>
        switch (endian) {
        | Some((`LE | `BE) as endian) =>
          [@implicit_arity] Ok(unsigned, `String16(endian))
        | _ => Error(`Unsupported_type)
        }
      | #numeric as numeric =>
        let (arithmetic, s) = {
          let (arithmetic, s) = span(~min=1, ~max=1, ~sat=Arithmetic.is, s);
          if (length(arithmetic) == 1) {
            (Some(to_string(arithmetic)), s);
          } else {
            (None, s);
          };
        };
        switch (arithmetic) {
        | Some(arithmetic) =>
          Integer.parse(s)
          >>= (
            ((with_val, empty)) =>
              if (is_empty(empty)) {
                [@implicit_arity]
                Ok(
                  unsigned,
                  `Numeric((
                    endian,
                    numeric,
                    Some(Arithmetic.of_string(~with_val, arithmetic)),
                  )),
                );
              } else {
                Error(`Unexpected_trailer(empty));
              }
          )
        | None =>
          [@implicit_arity] Ok(unsigned, `Numeric((endian, numeric, None)))
        };
      }
  );
};

let x = v("x");

let is_x = s => {
  let s = trim(~drop=is_wsp, s);
  equal_bytes(s, x);
};

let is_modifier = s =>
  switch (head(s)) {
  | Some('G' .. 'Z')
  | Some('g' .. 'z') => length(s) == 1
  | _ => false
  };

let parse_test = s =>
  if (is_x(s)) {
    Ok(`True);
  } else {
    let (comparison, s) = span(~min=1, ~max=1, ~sat=Comparison.is, s);
    let comparison =
      if (is_empty(comparison)) {
        "=";
      } else {
        to_string(comparison);
      };
    let s = trim(s);
    /* XXX(dinosaure): we can have [< 10], so [s = " 10"], we
       must [trim] it. */
    let parse_string = s => {
      let lexbuf = Lexing.from_string(to_string(s));
      let buf = Buffer.create(length(s));
      let contents = Lexer.string(buf, lexbuf);
      /* escape */
      Ok(`String(Comparison.of_string(~with_val=contents, comparison)));
    };
    switch (Number.parse(s)) {
    | [@implicit_arity] Ok(v, empty) =>
      if (is_empty(empty) || is_modifier(empty)) {
        Ok(
          `Numeric(
            Comparison.of_string(~with_val=(v, to_string(s)), comparison),
          ),
        );
      } else {
        parse_string(s);
      }
    | Error(`Invalid_number(_) | `Empty) =>
      if (is_empty(s)) {
        Ok(
          `Numeric(
            Comparison.of_string(
              ~with_val=(Number.int64(0L), ""),
              comparison,
            ),
          ),
        );
      } else {
        parse_string(s);
      }
    };
  };

let parse_message = s =>
  switch (head(s)) {
  | Some('\b') => Ok(`No_space((to_string <.> tail)(s)))
  | Some('\\') =>
    switch ((head <.> tail)(s)) {
    | Some('b') => Ok(`No_space((to_string <.> tail <.> tail)(s)))
    | _ => Ok(`Space(to_string(s)))
    }
  | _ => Ok(`Space(to_string(s)))
  };

let parse_strength = s => {
  let s = trim(~drop=is_wsp, s);
  let (arithmetic, s) = span(~min=1, ~max=1, ~sat=Arithmetic.is, s);
  let s = trim(~drop=is_wsp, s);
  if (length(arithmetic) == 1) {
    Integer.parse(s)
    >>= (
      ((with_val, _empty)) =>
        Ok(Arithmetic.of_string(~with_val, to_string(arithmetic)))
    );
  } else {
    Error(`Invalid_strength);
  };
};

let parse_use = (offset, s) => {
  let buf = Buffer.create(16);
  let lexbuf = Lexing.from_string(to_string(s));
  let name = Lexer.string(buf, lexbuf);
  let empty = with_range(~first=Lexing.lexeme_end(lexbuf), s);
  if (name != "" && is_empty(empty)) {
    switch (string_head(name)) {
    | Some('^') => Ok(`Use((offset, true, string_tail(name))))
    | _ => Ok(`Use((offset, false, name)))
    };
  } else {
    Error(`Invalid_use_command);
  };
};

let hws = v("\t");

let wsp = v(" ");

type rule = (offset, kind, test, message)

and offset = (
  int,
  [
    | `Abs(int64)
    | `Rel(int64)
    | `Ind(
        [ | `Abs | `Rel],
        (
          [ | `Abs(int64) | `Rel(int64)],
          option(Size.t),
          option(Arithmetic.t([ | `Dir(int64) | `Ind(int64)])),
        ),
      )
  ],
)

and kind = (
  bool,
  [
    | `Numeric(
        option([ | `BE | `LE | `ME]),
        numeric,
        option(Arithmetic.t(int64)),
      )
    | `Default
    | `Clear
    | `Indirect(bool)
    | `Regex(option((bool, bool, bool, int64)))
    | `String16([ | `BE | `LE])
    | `String8(option((bool, bool, bool, bool)))
    | `Search(option((list(search_flag), option(int64))))
  ],
)

and search_flag = [ | `t | `T | `b | `B | `c | `C | `w | `W]

and test = [
  | `True
  | `Numeric(Comparison.t((Number.t, string)))
  | `String(Comparison.t(string))
]

and message = [ | `No_space(string) | `Space(string)];

type line = [
  | `Comment
  | `Apple(string)
  | `Ext(string)
  | `Mime(string)
  | `Strength(Arithmetic.t(int64))
  | `Rule(rule)
  | `Name(offset, string)
  | `Guid(offset, string)
  | `Use(offset, bool, string)
];

let escape = v("\\");

/* TODO(dinosaure): we should clear semantic of this function:
      "x\t" returns ("x", [])
   */
let best_effort = s =>
  switch (cuts(~empty=false, ~sep=hws, s)) {
  | [] => Error(`Missing_test(s))
  | [test0] =>
    switch (cuts(~empty=false, ~sep=wsp, s)) {
    | [] => [@implicit_arity] Ok(test0, [])
    | [test1] =>
      if (is_empty(test0)) {
        [@implicit_arity] Ok(test1, []);
      } else {
        [@implicit_arity] Ok(test0, []);
      }
    | [hd, ...tl] =>
      let (test, message) =
        List.fold_left(
          ((test, message), elt) =>
            switch (test, message) {
            | ([], []) => ([elt], [])
            | ([hd, ..._] as test, []) =>
              if (is_suffix(~affix=escape, hd)) {
                ([elt, ...test], []);
              } else {
                (test, [elt]);
              }
            | (_, message) => (test, [elt, ...message])
            },
          ([hd], []),
          tl,
        );
      let test = concat(~sep=wsp, List.rev(test));
      [@implicit_arity] Ok(test, message);
    }
  | [test, ...message] => [@implicit_arity] Ok(test, message)
  };

type error = [
  | `Empty
  | `Missing_test(s)
  | `Unmatched_parenthesis(s)
  | `Unexpected_trailer(s)
  | `Invalid_number(s)
  | `Invalid_integer(string)
  | `Invalid_strength
  | `Invalid_type(s)
  | `No_prefix(s, s)
  | `Unsupported_type
  | `Invalid_use_command
];

let parse_line = (line): result(line, [> error]) =>
  switch (string_head(line)) {
  | None
  | Some('#') => Ok(`Comment)
  | _ =>
    let s = v(line);
    let (verb, s) = span(~sat=(!) <.> is_wsp, s);
    let s = drop(~sat=is_wsp, s);

    switch (to_string(verb)) {
    | "!:apple" => Ok(`Apple(to_string(s)))
    | "!:ext" => Ok(`Ext(to_string(s)))
    | "!:mime" => Ok(`Mime(to_string(s)))
    | "!:strength" =>
      parse_strength(s) >>= (strength => Ok(`Strength(strength)))
    | _ =>
      parse_offset(verb)
      >>= (
        offset => {
          let (ty, s) = span(~sat=(!) <.> is_wsp, s);
          let s = drop(~sat=is_wsp, s);
          switch (to_string(ty)) {
          | "name" => Ok(`Name((offset, to_string(s))))
          | "guid" => Ok(`Guid((offset, to_string(s))))
          | "use" => parse_use(offset, trim(~drop=is_wsp, s))
          | _ =>
            parse_type(ty)
            >>= (
              ty =>
                best_effort(s)
                >>= (
                  ((test, message)) =>
                    parse_test(test)
                    >>= (
                      test =>
                        parse_message(concat(~sep=wsp, message))
                        >>= (
                          message => Ok(`Rule((offset, ty, test, message)))
                        )
                    )
                )
            )
          };
        }
      )
    };
  };

let pp_error = (ppf, err) => {
  let pp = Format.fprintf;
  let to_string = Sub.to_string;

  switch (err) {
  | `Invalid_number(s) => pp(ppf, "Invalid number %S", to_string(s))
  | `Invalid_integer(s) => pp(ppf, "Invalid integer %S", s)
  | `Invalid_strength => pp(ppf, "Invalid strength")
  | `Invalid_type(s) => pp(ppf, "Invalid type %S", to_string(s))
  | `No_prefix(prefix, s) =>
    pp(ppf, "Expected prefix %S on %S", to_string(prefix), to_string(s))
  | `Unsupported_type => pp(ppf, "Unsupported type")
  | `Unmatched_parenthesis(s) =>
    pp(ppf, "Unmatched parenthesis %S", to_string(s))
  | `Unexpected_trailer(s) => pp(ppf, "Unexpected_trailer %S", to_string(s))
  | `Missing_test(s) => pp(ppf, "Missing test %S", to_string(s))
  | `Empty => pp(ppf, "Empty string")
  | `Invalid_use_command => pp(ppf, "Invalid #use command")
  };
};

let parse_in_channel = ic => {
  let rec go = acc =>
    switch (input_line(ic)) {
    | line => parse_line(line) >>= (v => go([v, ...acc]))
    | exception End_of_file => Ok(List.rev(acc))
    };
  go([]);
};
