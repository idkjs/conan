type s = Sub.t;

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

let pp_error: (Format.formatter, error) => unit;

let parse_line: string => result(line, error);

let parse_in_channel: in_channel => result(list(line), [> error]);
