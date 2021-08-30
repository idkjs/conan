let (/) = Filename.concat;

let conan_database =
  switch (Sys.getenv("CONAN_DATABASE")) {
  | path => path
  | exception Not_found => "./examples/"
  };

let database = {
  let rec go = tree =>
    fun
    | [] => tree
    | [filename, ...rest] => {
        let ic = open_in(conan_database / filename);
        let rs = Conan.Parse.parse_in_channel(ic);
        close_in(ic);
        switch (rs) {
        | Ok(lines) =>
          let (_, tree) =
            List.fold_left(
              ((line, tree), v) =>
                (succ(line), Conan.Tree.append(~filename, ~line, tree, v)),
              (1, tree),
              lines,
            );
          go(tree, rest);
        | _ => go(tree, rest)
        };
      };
  Sys.readdir(conan_database)
  |> Array.to_list
  |> go(Conan.Tree.empty)
  |> (tree => Conan.Process.database(~tree));
};

open Crowbar;

let () =
  add_test(~name="lint (assert false / exception)", [bytes]) @@
  (
    str =>
      switch (Conan_string.run(~database, str)) {
      | Error(`Msg(err)) =>
        Fmt.epr("Got an error for the given input: %s.\n%!", err)
      | Ok(metadata) =>
        switch (
          Conan.Metadata.mime(metadata),
          Conan.Metadata.output(metadata),
        ) {
        | (Some(mime), None) => Fmt.pr("Found MIME type: %S.\n%!", mime)
        | (None, Some(output)) =>
          Fmt.pr("Found a solution for the given input: %S.\n%!", output)
        | (Some(mime), Some(output)) => Fmt.pr("%S (%S)\n%!", output, mime)
        | (None, None) => ()
        }
      }
  );
