let database: (~directory: string) => Conan.Tree.t;

let run_with_tree:
  (Conan.Tree.t, string) => result(Conan.Metadata.t, [> | `Msg(string)]);

let run:
  (~database: string, string) => result(Conan.Metadata.t, [> | `Msg(string)]);
