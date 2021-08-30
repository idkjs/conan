type t = {
  output: option(string),
  mime: option(string),
};

let pf = Format.fprintf;

let pp = (ppf, t) => {
  let pp_option = (pp_val, ppf) =>
    fun
    | Some(v) => pp_val(ppf, v)
    | None => ();
  let pp_string = (ppf, v) => pf(ppf, "%s", v);
  pf(
    ppf,
    "{ @[<hov>output= %S;@ mime= %a;@] }",
    Option.value(~default="", t.output),
    pp_option(pp_string),
    t.mime,
  );
};

let with_mime = (mime, t) => {...t, mime: Some(mime)};

let with_output = (output, t) =>
  if (output != "") {
    switch (t.output) {
    | Some(_) => {...t, output: Some(output)}
    | None when output.[0] == ' ' => {
        ...t,
        output: Some(String.sub(output, 1, String.length(output) - 1)),
      }
    | None => {...t, output: Some(output)}
    };
  } else {
    t;
  };

let output = ({output, _}) => output;

let mime = ({mime, _}) => mime;

let empty = {output: None, mime: None};

let concat = (a0, a1) => {
  let output =
    switch (a0.output, a1.output) {
    | (None, Some(v)) => Some(v)
    | (Some(v), None) => Some(v)
    | (Some(a), Some(b)) => Some(a ++ b)
    | (None, None) => None
    };
  let mime =
    switch (a0.mime, a1.mime) {
    | (None, Some(v)) => Some(v)
    | (Some(v), None) => Some(v)
    | (Some(v), Some(_)) => Some(v)
    | (None, None) => None
    };
  {output, mime};
};
