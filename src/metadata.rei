type t;

let pp: (Format.formatter, t) => unit;

let with_mime: (string, t) => t;

let with_output: (string, t) => t;

let output: t => option(string);

let mime: t => option(string);

let concat: (t, t) => t;

let empty: t;
