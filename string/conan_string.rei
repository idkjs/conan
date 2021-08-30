let run:
  (~database: Conan.Process.database, string) =>
  result(Conan.Metadata.t, [> | `Msg(string)]);
