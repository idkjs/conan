module Log = (val Logs.src_log(Logs.Src.create("conan-process")));

let invalid_arg = fmt => Format.kasprintf(invalid_arg, fmt);

let (<.>) = (f, g, x) => f(g(x));

let ok = x => Ok(x);

let reword_error = f =>
  fun
  | Ok(_) as v => v
  | Error(err) => Error(f(err));

open Sigs;

let process_fmt:
  type v. (Metadata.t, Ty.t(_, v), Tree.fmt(v), v) => Metadata.t =
  (m, _, tree_fmt, v) => {
    let buf = Buffer.create(16);
    let ppf = Format.formatter_of_buffer(buf);
    Fmt.keval(
      Pps.v,
      ppf,
      Fmt.([String(Nop)] ^^ Tree.fmt(tree_fmt, ())),
      ppf => {
        Format.fprintf(ppf, "%!");
        Metadata.with_output(Buffer.contents(buf), m);
      },
      Option.value(~default="", Metadata.output(m)),
      v,
    );
  };

let process:
  type s fd error.
    (
      scheduler(s),
      syscall(fd, error, s),
      fd,
      int64,
      Metadata.t,
      Tree.operation
    ) =>
    io(
      result(
        (int64, Metadata.t),
        [> | `Syscall(error) | `Invalid_test | `No_process],
      ),
      s,
    ) =
  ({bind, return} as scheduler, syscall, fd, abs_offset, metadata, operation) => {
    Log.debug(m =>
      m("Process the operation: %a", Tree.pp_operation, operation)
    );
    Log.debug(m => m("Current metadata: %a", Metadata.pp, metadata));
    switch (operation) {
    | Tree.Name(_) => return(Error(`No_process))
    | Tree.Use(_) => return(Error(`No_process))
    | Tree.MIME(v) =>
      return(
        [@implicit_arity] Ok(abs_offset, Metadata.with_mime(v, metadata)),
      )
    | [@implicit_arity] Tree.Rule(offset, ty, test, fmt) =>
      let (>>=) = bind;
      let (>|=) = (x, f) => x >>= (x => return(f(x)));
      let (>?=) = (x, f) =>
        x
        >>= (
          fun
          | Ok(x) => f(x)
          | Error(err) => return(Error(err))
        );
      Offset.process(scheduler, syscall, fd, offset, abs_offset)
      >|= reword_error(err => `Syscall(err))
      >?= (
        abs_offset =>
          Ty.process(scheduler, syscall, fd, abs_offset, ty)
          >?= (
            v => {
              Log.debug(m =>
                m("Test %a %a.", Test.pp, test, Ty.pp_of_result(ty), v)
              );
              switch (Test.process(ty, test, v)) {
              | Some(v) =>
                let pp = Ty.pp_of_result(ty);
                Log.debug(m => m("Test pass with %a!", pp, v));
                let metadata = process_fmt(metadata, ty, fmt, v);
                return([@implicit_arity] Ok(abs_offset, metadata));
              | None => return(Error(`Invalid_test))
              };
            }
          )
      );
    };
  };

let descending_walk =
    (
      {bind, return} as scheduler,
      syscall,
      db,
      fd,
      abs_offset,
      metadata,
      root,
    ) => {
  let (>>=) = bind;

  /* ugly, as f*ck! */
  let rec go = (~level, syscall, abs_offset, candidate0) =>
    fun
    | Tree.Done => return(candidate0)
    | Tree.Node(lst) => {
        let lst =
          List.rev_map(((elt, sub)) => (Tree.operation(elt), sub), lst);
        iter(~level, [], syscall, abs_offset, candidate0, lst);
      }
  and iter = (~level, results, syscall, abs_offset, candidate1) =>
    fun
    | [] => return(candidate1)
    | [(Tree.Name(_), _), ...rest] =>
      iter(~level, results, syscall, abs_offset, candidate1, rest)
    | [(Tree.Use({offset, invert: false, name}), Tree.Done), ...rest] =>
      Offset.process(scheduler, syscall, fd, offset, abs_offset)
      >>= (
        fun
        | Ok(shift) => {
            let seek = (fd, abs_offset, where) =>
              syscall.seek(fd, Int64.add(abs_offset, shift), where);
            if (!Hashtbl.mem(db, name)) {
              invalid_arg("%s does not exist", name);
            };
            let tree = Hashtbl.find(db, name);
            go(
              {...syscall, seek},
              ~level=succ(level),
              0L,
              /* XXX(dinosaure): or [abs_offset]? */ candidate1,
              tree,
            )
            >>= (
              candidate2 =>
                iter(~level, results, syscall, abs_offset, candidate2, rest)
            );
          }
        | Error(_) =>
          iter(~level, results, syscall, abs_offset, candidate1, rest)
      )
    | [(Tree.Use({offset, invert: true, name}), Tree.Done), ...rest] =>
      Offset.process(scheduler, syscall, fd, offset, abs_offset)
      >>= (
        fun
        | Ok(shift) => {
            let seek = (fd, abs_offset, where) =>
              syscall.seek(fd, Int64.add(abs_offset, shift), where);
            if (!Hashtbl.mem(db, name)) {
              invalid_arg("%s does not exist", name);
            };
            let tree = Hashtbl.find(db, name);
            go(
              Size.invert(scheduler, {...syscall, seek}),
              ~level=succ(level),
              0L,
              /* XXX(dinosaure): or [abs_offset]? */ candidate1,
              tree,
            )
            >>= (
              candidate2 =>
                iter(~level, results, syscall, abs_offset, candidate2, rest)
            );
          }
        | Error(_) =>
          iter(~level, results, syscall, abs_offset, candidate1, rest)
      )
    | [
        (
          [@implicit_arity] Tree.Rule(offset, Ty.Indirect(`Rel), _, _),
          Tree.Done,
        ),
        ...rest,
      ] =>
      Offset.process(scheduler, syscall, fd, offset, abs_offset)
      >>= (
        fun
        | Ok(shift) => {
            let seek = (fd, abs_offset, where) =>
              syscall.seek(fd, Int64.add(abs_offset, shift), where);
            let metadata = Metadata.empty;
            go(
              {...syscall, seek},
              ~level=succ(level),
              abs_offset,
              metadata,
              root,
            )
            >>= (
              metadata => {
                let candidate1 = Metadata.concat(candidate1, metadata);
                iter(
                  ~level,
                  [candidate1, ...results],
                  syscall,
                  Int64.add(abs_offset, shift),
                  candidate1,
                  rest,
                );
              }
            );
          }
        | Error(_) =>
          iter(~level, results, syscall, abs_offset, candidate1, rest)
      )
    | [
        ([@implicit_arity] Tree.Rule(_, Ty.Default, _, _) as operation, tree),
        ...rest,
      ] =>
      switch (results) {
      | [_, ..._] =>
        iter(~level, results, syscall, abs_offset, candidate1, rest)
      | [] =>
        process(scheduler, syscall, fd, abs_offset, candidate1, operation)
        >>= (
          fun
          | [@implicit_arity] Ok(abs_offset, candidate2) =>
            go(syscall, ~level=succ(level), abs_offset, candidate2, tree)
            >>= (
              candidate3 =>
                iter(
                  ~level,
                  [candidate3, ...results],
                  syscall,
                  abs_offset,
                  candidate3,
                  rest,
                )
            )
          | Error(_) =>
            iter(~level, [], syscall, abs_offset, candidate1, rest)
        )
      }
    | [
        ([@implicit_arity] Tree.Rule(_, Ty.Clear, _, _) as _operation, _),
        ...rest,
      ] =>
      iter(~level, [], syscall, abs_offset, candidate1, rest)
    /* TODO: compute [operation]? */
    | [(operation, tree), ...rest] =>
      process(scheduler, syscall, fd, abs_offset, candidate1, operation)
      >>= (
        fun
        | [@implicit_arity] Ok(abs_offset, candidate1) =>
          go(syscall, ~level=succ(level), abs_offset, candidate1, tree)
          >>= (
            candidate2 =>
              iter(
                ~level,
                [candidate2, ...results],
                syscall,
                abs_offset,
                candidate2,
                rest,
              )
          )
        | Error(_) =>
          iter(~level, results, syscall, abs_offset, candidate1, rest)
      );

  go(~level=0, syscall, abs_offset, metadata, root);
};

type database = (Hashtbl.t(string, Tree.t), Tree.t);

let rec fill_db = db =>
  fun
  | Tree.Done => ()
  | Tree.Node(lst) => {
      let rec go = (
        fun
        | [] => ()
        | [([@implicit_arity] Tree.Name(_, name), tree), ...rest] => {
            /* XXX(dinosaure): /offset/ name value
               should appear only at the first level. */
            Hashtbl.add(db, name, tree);
            fill_db(db, tree);
            go(rest);
          }
        | [(_, tree), ...rest] => {
            fill_db(db, tree);
            go(rest);
          }
      );
      go(List.rev_map(((elt, sub)) => (Tree.operation(elt), sub), lst));
    };

let database = (~tree): database => {
  let db = Hashtbl.create(0x10);
  fill_db(db, tree);
  (db, tree);
};

let descending_walk = (scheduler, syscall, fd, (db, tree)) =>
  descending_walk(scheduler, syscall, db, fd, 0L, Metadata.empty, tree);

let rec ascending_walk =
        ({bind, return} as scheduler, syscall, db, fd, results, queue) => {
  let (>>=) = bind;

  switch (Queue.pop(queue)) {
  | (_, candidate, Tree.Done) =>
    ascending_walk(
      scheduler,
      syscall,
      db,
      fd,
      [candidate, ...results],
      queue,
    )
  | (abs_offset, candidate, Tree.Node(lst)) =>
    let lst =
      List.rev_map(((elt, sub)) => (Tree.operation(elt), sub), lst);
    let rec go = candidate => (
      fun
      | [] => return()
      | [([@implicit_arity] Tree.Name(_, name), tree), ...rest] => {
          Hashtbl.add(db, name, tree);
          go(candidate, rest);
        }
      | [(Tree.Use({name, _}), Tree.Done), ...rest] => {
          let tree = Hashtbl.find(db, name);
          Queue.push((abs_offset, candidate, tree), queue);
          go(candidate, rest);
        }
      | [(operation, tree), ...rest] =>
        process(scheduler, syscall, fd, abs_offset, candidate, operation)
        >>= (
          fun
          | [@implicit_arity] Ok(abs_offset, candidate) => {
              Queue.push((abs_offset, candidate, tree), queue);
              go(candidate, rest);
            }
          | Error(_) => go(candidate, rest)
        )
    );
    go(candidate, lst)
    >>= (() => ascending_walk(scheduler, syscall, db, fd, results, queue));
  | exception Queue.Empty => return(List.rev(results))
  };
};

let ascending_walk = (scheduler, syscall, fd, tree) => {
  let queue = Queue.create();
  let db = Hashtbl.create(0x10);
  Queue.push((0L, Metadata.empty, tree), queue);
  ascending_walk(scheduler, syscall, db, fd, [], queue);
};
