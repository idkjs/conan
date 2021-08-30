open Conan_unix;

let previous = ref(Mtime.of_uint64_ns(0L));

let (>>=) = (x, f) =>
  switch (x) {
  | Ok(x) => f(x)
  | Error(err) => Error(err)
  };

let run = (~fmt=`Usual, filename) =>
  run(~database="examples", filename)
  >>= (
    result =>
      switch (fmt) {
      | `Usual =>
        switch (Conan.Metadata.output(result)) {
        | Some(output) =>
          Format.printf("%s: %s\n%!", filename, output);
          Ok();
        | None => Error(`Not_found)
        }
      | `MIME =>
        switch (Conan.Metadata.mime(result)) {
        | None => Error(`Not_found)
        | Some(mime) =>
          Format.printf("%s\n%!", mime);
          Ok();
        }
      }
  );

let pp_error = ppf =>
  fun
  | #Conan.Parse.error as err => Conan.Parse.pp_error(ppf, err)
  | `Msg(err) => Format.fprintf(ppf, "%s", err)
  | `Not_found => Format.fprintf(ppf, "Not found");

let mime = ref(false);

let filename = ref(None);

let anonymous_argument = v =>
  switch (filename^) {
  | None => filename := Some(v)
  | Some(_) => ()
  };

let usage = Format.asprintf("%s [--mime] filename\n%!", Sys.argv[0]);

let spec = [
  (
    "--mime",
    Arg.Set(mime),
    "Causes the file command to output mime type strings rather than the more traditional human readable ones. Thus it may say 'text/plain; charset=ascii' rather than 'ASCII text'",
  ),
];

let exit_success = 0;

let exit_failure = 1;

let () = {
  Arg.parse(spec, anonymous_argument, usage);
  switch (filename^) {
  | None =>
    Format.eprintf("%s", usage);
    exit(exit_failure);
  | Some(filename) =>
    let fmt =
      if (mime^) {
        `MIME;
      } else {
        `Usual;
      };
    switch (run(~fmt, filename)) {
    | Ok () => exit(exit_success)
    | Error(err) => Format.eprintf("%s: %a.\n%!", Sys.argv[0], pp_error, err)
    };
  };
};
