open Sigs;

[@unboxed]
type finder = {
  f:
    't 'fd 'error.
    (
      scheduler('t),
      ~get: ('fd, ~pos: int64) => io(result(char, 'error), 't),
      ~ln: int64,
      'fd
    ) =>
    io(result(list(int64), 'error), 't),

};

let find_one = (~pattern) => {
  let nlen = String.length(pattern);
  let next = Array.make(nlen, 0);
  let i = ref(1);
  let j = ref(0);
  if (nlen > 1) {
    while (i^ < nlen - 1) {
      if (pattern.[i^] == pattern.[j^]) {
        incr(i);
        incr(j);
        next[i^] = j^;
      } else if (j^ == 0) {
        incr(i);
      } else {
        j := next[j^];
      };
    };
  };
  {
    f: ({bind, return}, ~get, ~ln, fd) => {
      let (>>=) = bind;
      let (>>?) = (x, f) =>
        x
        >>= (
          fun
          | Ok(x) => f(x)
          | Error(_) as err => return(err)
        );
      let rec go = (pos, idx) =>
        if (idx < nlen && pos < ln) {
          get(fd, ~pos)
          >>? (
            chr =>
              if (pattern.[idx] == chr) {
                go(Int64.succ(pos), succ(idx));
              } else if (idx == 0) {
                go(Int64.succ(pos), idx);
              } else {
                go(pos, next[idx]);
              }
          );
        } else if (idx == nlen) {
          return(Ok([Int64.sub(pos, Int64.of_int(nlen))]));
        } else {
          return(Ok([]));
        };
      go(0L, 0);
    },
  };
};
