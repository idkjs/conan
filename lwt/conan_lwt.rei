let run:
  (~database: Conan.Process.database, unit => Lwt.t(option(string))) =>
  Lwt.t(result(Conan.Metadata.t, [> | `Msg(string)]));
