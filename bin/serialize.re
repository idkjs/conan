let (/) = Filename.concat;

let identity = x => x;

let ocamlify = s => {
  let b = Buffer.create(String.length(s));
  String.iter(
    fun
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c =>
      Buffer.add_char(b, c)
    | '-'
    | '.' => Buffer.add_char(b, '_')
    | _ => (),
    s,
  );
  let s' = Buffer.contents(b);
  if (String.length(s') == 0 || '0' <= s'.[0] && s'.[0] <= '9') {
    raise(Invalid_argument(s));
  };
  s';
};

let reserved_keyword = [("virtual", "virtual'")];

let ocamlify = s =>
  switch (List.assoc(s, reserved_keyword)) {
  | s' => s'
  | exception Not_found => ocamlify(s)
  };

let serialize_into_multiple_files = (directory, filename, tree) => {
  let rec go = ((idx, acc)) =>
    fun
    | [] => List.rev(acc)
    | [(elt, tree), ...r] => {
        let filename' =
          Format.asprintf("%s_%03d.ml", ocamlify(filename), idx);
        let oc = open_out(directory / filename');
        let ppf = Format.formatter_of_out_channel(oc);
        Format.fprintf(
          ppf,
          "let tree = @[<2>%a@]\n%!",
          Conan.Tree.serialize,
          tree,
        );
        close_out(oc);
        go((succ(idx), [elt, ...acc]), r);
      };
  [@warning "-8"]
  let Conan.Tree.Node(lst) = tree;
  let elts = go((0, []), lst);
  let filename' = Format.asprintf("%s.ml", ocamlify(filename));
  let oc = open_out(directory / filename');
  let ppf = Format.formatter_of_out_channel(oc);
  List.iteri(
    (idx, elt) =>
      Format.fprintf(
        ppf,
        "let tree_%03d = (@[<1>%a,@ %s_%03d.tree@])\n%!",
        idx,
        Conan.Tree.serialize_elt,
        elt,
        String.capitalize_ascii(ocamlify(filename)),
        idx,
      ),
    elts,
  );
  Format.fprintf(
    ppf,
    "let tree = Conan.Tree.Unsafe.node @[%a@]\n%!",
    Conan.Serialize.(list(fmt("tree_%03d"))),
    List.init(List.length(elts), identity),
  );
  close_out(oc);
};

let serialize = (directory, filename) => {
  let ic = open_in(directory / filename);
  let rs = Conan.Parse.parse_in_channel(ic);
  close_in(ic);
  switch (rs) {
  | Ok(lines) =>
    let (_, tree) =
      List.fold_left(
        ((line, tree), v) =>
          (succ(line), Conan.Tree.append(~filename, ~line, tree, v)),
        (1, Conan.Tree.empty),
        lines,
      );
    if (Conan.Tree.weight(tree) >= 2000) {
      serialize_into_multiple_files(directory, filename, tree);
    } else {
      let filename' = ocamlify(filename) ++ ".ml";
      let oc = open_out(directory / filename');
      let ppf = Format.formatter_of_out_channel(oc);
      Format.fprintf(
        ppf,
        "let tree = @[<2>%a@]\n%!",
        Conan.Tree.serialize,
        tree,
      );
      close_out(oc);
    };
  | Error(_err) =>
    let filename' = ocamlify(filename) ++ ".ml";
    let oc = open_out(directory / filename');
    let ppf = Format.formatter_of_out_channel(oc);
    Format.fprintf(ppf, "let tree = Conan.Tree.empty\n%!");
    close_out(oc);
  };
};

let run = directory => {
  let files = Sys.readdir(directory);
  let files = Array.to_list(files);
  List.iter(serialize(directory), files);
  let files = List.map(ocamlify, files);
  let oc = open_out(directory / "conan_database.ml");
  let ppf = Format.formatter_of_out_channel(oc);
  List.iter(
    filename =>
      Format.fprintf(
        ppf,
        "let %s = %s.tree\n%!",
        filename,
        String.capitalize_ascii(filename),
      ),
    files,
  );
  Format.fprintf(
    ppf,
    "let tree = List.fold_left Conan.Tree.merge Conan.Tree.empty @[%a@]\n%!",
    Conan.Serialize.(list(fmt("%s"))),
    files,
  );
  close_out(oc);
};

let directory = ref(None);

let anonymous_argument = v =>
  switch (directory^) {
  | None => directory := Some(v)
  | Some(_) => ()
  };

let usage = Format.asprintf("%s database\n%!", Sys.argv[0]);

let exit_success = 0;

let exit_failure = 1;

let () = {
  Arg.parse([], anonymous_argument, usage);
  switch (directory^) {
  | None =>
    Format.eprintf("%s", usage);
    exit(exit_failure);
  | Some(directory) =>
    run(directory);
    exit(exit_success);
  };
};
