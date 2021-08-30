/* Copyright (c) 2015 Daniel C. Bünzli. All rights reserved. */

type t = {
  start: int,
  stop: int,
  s: string,
};

let v = (~start=0, ~stop=?, s) => {
  let s_len = String.length(s);
  let stop =
    switch (stop) {
    | None => s_len
    | Some(stop) => stop
    };
  if (start < 0 || stop > s_len || stop < start) {
    invalid_arg("Out of bounds");
  };
  {start, stop, s};
};

let empty = {start: 0, stop: 0, s: ""};

let length = ({start, stop, _}) => stop - start;

let head = (~rev=false, {s, start, stop}) =>
  if (start == stop) {
    None;
  } else {
    Some(
      s.[if (rev) {
           stop - 1;
         } else {
           start;
         }],
    );
  };

let tail = (~rev=false, {s, start, stop} as sub) =>
  if (start == stop) {
    sub;
  } else if (rev) {
    {s, start, stop: stop - 1};
  } else {
    {s, start: start + 1, stop};
  };

let is_empty = ({start, stop, _}) => stop - start == 0;

let is_prefix =
    (
      ~affix as {s: affix, start: astart, _} as affix_sub,
      {s, start: sstart, _} as s_sub,
    ) => {
  let len_a = length(affix_sub);
  let len_s = length(s_sub);
  if (len_a > len_s) {
    false;
  } else {
    let max_zidx /* zero based idx */ = len_a - 1;
    let rec loop = i =>
      if (i > max_zidx) {
        true;
      } else if (affix.[astart + i] != s.[sstart + i]) {
        false;
      } else {
        loop(i + 1);
      };
    loop(0);
  };
};

let is_suffix =
    (
      ~affix as {s: affix, stop: astop, _} as affix_sub,
      {s, stop: sstop, _} as s_sub,
    ) => {
  let len_a = length(affix_sub);
  let len_s = length(s_sub);
  if (len_a > len_s) {
    false;
  } else {
    let max_zidx /* zero based idx */ = len_a - 1;
    let max_idx_a = astop - 1;
    let max_idx_s = sstop - 1;
    let rec loop = i =>
      if (i > max_zidx) {
        true;
      } else if (affix.[max_idx_a - i] != s.[max_idx_s - i]) {
        false;
      } else {
        loop(i + 1);
      };
    loop(0);
  };
};

let fspan = (~min, ~max, ~sat, {s, start, stop} as sub) =>
  if (min < 0) {
    invalid_arg("fspan");
  } else if (max < 0) {
    invalid_arg("fspan");
  } else if (min > max || max == 0) {
    ({s, start, stop: start}, sub);
  } else {
    let max_idx = stop - 1;
    let max_idx = {
      let k = start + max - 1;
      if (k > max_idx || k < 0) {
        max_idx;
      } else {
        k;
      };
    };
    let need_idx = start + min;
    let rec loop = i =>
      if (i <= max_idx && sat(s.[i])) {
        loop(i + 1);
      } else if (i < need_idx || i == 0) {
        ({s, start, stop: start}, sub);
      } else if (i == stop) {
        (sub, {s, start: stop, stop});
      } else {
        ({s, start, stop: i}, {s, start: i, stop});
      };
    loop(start);
  };

let rspan = (~min, ~max, ~sat, {s, start, stop} as sub) =>
  if (min < 0) {
    invalid_arg("rspan");
  } else if (max < 0) {
    invalid_arg("rspan");
  } else if (min > max || max == 0) {
    (sub, {s, start: stop, stop});
  } else {
    let max_idx = stop - 1;
    let min_idx = {
      let k = stop - max;
      if (k < start) {
        start;
      } else {
        k;
      };
    };
    let need_idx = stop - min - 1;
    let rec loop = i =>
      if (i >= min_idx && sat(s.[i])) {
        loop(i - 1);
      } else if (i > need_idx || i == max_idx) {
        (sub, {s, start: stop, stop});
      } else if (i == start - 1) {
        ({s, start, stop: start}, sub);
      } else {
        ({s, start, stop: i + 1}, {s, start: i + 1, stop});
      };
    loop(max_idx);
  };

let span = (~rev=false, ~min=0, ~max=max_int, ~sat=_ => true, sub) =>
  rev ? rspan(~min, ~max, ~sat, sub) : fspan(~min, ~max, ~sat, sub);

let to_string = ({s, start, stop}) =>
  if (start == stop) {
    "";
  } else if (start == 0 && stop == String.length(s)) {
    s;
  } else {
    String.sub(s, start, stop - start);
  };

let rebase = ({start, stop, _} as sub) => {
  s: to_string(sub),
  start: 0,
  stop: stop - start,
};

let concat = (~sep as {s: sep, start: sep_start, _} as sep_sub=empty) =>
  fun
  | [] => empty
  | [s] => rebase(s)
  | [{s, start, _} as sub, ...ss] => {
      let sub_len = length(sub);
      let sep_len = length(sep_sub);
      let rec cat_len = (sep_count, l, ss) =>
        if (l < 0) {
          l;
        } else {
          switch (ss) {
          | [s, ...ss] => cat_len(sep_count + 1, l + length(s), ss)
          | [] =>
            if (sep_len == 0) {
              l;
            } else {
              let max_sep_count = Sys.max_string_length / sep_len;
              if (sep_count < 0 || sep_count > max_sep_count) {
                (-1);
              } else {
                sep_count * sep_len + l;
              };
            }
          };
        };
      let cat_len = cat_len(0, sub_len, ss);
      if (cat_len < 0) {
        invalid_arg("concat");
      } else {
        let b = Bytes.create(cat_len);
        Bytes.blit_string(s, start, b, 0, sub_len);
        let rec loop = i => (
          fun
          | [] => Bytes.unsafe_to_string(b)
          | [{s: str, start: str_start, _} as str_sub, ...ss] => {
              let sep_pos = i;
              let str_pos = i + sep_len;
              let str_len = length(str_sub);
              Bytes.blit_string(sep, sep_start, b, sep_pos, sep_len);
              Bytes.blit_string(str, str_start, b, str_pos, str_len);
              loop(str_pos + str_len, ss);
            }
        );
        {s: loop(sub_len, ss), start: 0, stop: cat_len};
      };
    };

let exists = (sat, {s, start, stop}) => {
  let rec loop = i =>
    if (i > stop - 1) {
      false;
    } else if (sat(s.[i])) {
      true;
    } else {
      loop(succ(i));
    };

  loop(start);
};

let for_all = (sat, {s, start, stop}) => {
  let rec loop = i =>
    if (i > stop - 1) {
      true;
    } else if (sat(s.[i])) {
      loop(succ(i));
    } else {
      false;
    };

  loop(start);
};

let is_white =
  fun
  | ' '
  | '\t' .. '\r' => true
  | _ => false;

let trim = (~drop=is_white, {s, start, stop} as sub) => {
  let len = stop - start;
  if (len == 0) {
    sub;
  } else {
    let max_pos = stop;
    let max_idx = stop - 1;
    let rec left_pos = i =>
      if (i > max_idx) {
        max_pos;
      } else if (drop(s.[i])) {
        left_pos(i + 1);
      } else {
        i;
      };
    let rec right_pos = i =>
      if (i < start) {
        start;
      } else if (drop(s.[i])) {
        right_pos(i - 1);
      } else {
        i + 1;
      };
    let left = left_pos(start);
    if (left == max_pos) {
      {s, start: (start + stop) / 2, stop: (start + stop) / 2};
    } else {
      let right = right_pos(max_idx);
      if (left == start && right == max_pos) {
        sub;
      } else {
        {s, start: left, stop: right};
      };
    };
  };
};

let equal_bytes =
    (
      {s: s0, start: start0, stop: stop0},
      {s: s1, start: start1, stop: stop1},
    ) =>
  if (s0 === s1 && start0 == start1 && stop0 == stop1) {
    true;
  } else {
    let len0 = stop0 - start0;
    let len1 = stop1 - start1;
    if (len0 != len1) {
      false;
    } else {
      let max_zidx = len0 - 1;
      let rec loop = i =>
        if (i > max_zidx) {
          true;
        } else if (s0.[start0 + i] != s1.[start1 + i]) {
          false;
        } else {
          loop(i + 1);
        };
      loop(0);
    };
  };

let fcut =
    (~sep as {s: sep, start: sep_start, stop: sep_stop}, {s, start, stop}) => {
  let sep_len = sep_stop - sep_start;
  if (sep_len == 0) {
    invalid_arg("fcut");
  } else {
    let max_sep_zidx = sep_len - 1;
    let max_s_idx = stop - sep_len;
    let rec check_sep = (i, k) =>
      if (k > max_sep_zidx) {
        Some(({s, start, stop: i}, {s, start: i + sep_len, stop}));
      } else if (s.[i + k] == sep.[sep_start + k]) {
        check_sep(i, k + 1);
      } else {
        scan(i + 1);
      }
    and scan = i =>
      if (i > max_s_idx) {
        None;
      } else if (s.[i] == sep.[sep_start]) {
        check_sep(i, 1);
      } else {
        scan(i + 1);
      };
    scan(start);
  };
};

let rcut =
    (~sep as {s: sep, start: sep_start, stop: sep_stop}, {s, start, stop}) => {
  let sep_len = sep_stop - sep_start;
  if (sep_len == 0) {
    invalid_arg("rcut");
  } else {
    let max_sep_zidx = sep_len - 1;
    let max_s_idx = stop - 1;
    let rec check_sep = (i, k) =>
      if (k > max_sep_zidx) {
        Some(({s, start, stop: i}, {s, start: i + sep_len, stop}));
      } else if (s.[i + k] == sep.[sep_start + k]) {
        check_sep(i, k + 1);
      } else {
        rscan(i - 1);
      }
    and rscan = i =>
      if (i < start) {
        None;
      } else if (s.[i] == sep.[sep_start]) {
        check_sep(i, 1);
      } else {
        rscan(i - 1);
      };
    rscan(max_s_idx - max_sep_zidx);
  };
};

let cut = (~rev=false, ~sep, s) => rev ? rcut(~sep, s) : fcut(~sep, s);

let with_range = (~first=0, ~len=max_int, {s, start, stop}) =>
  if (len < 0) {
    invalid_arg("with_range");
  } else {
    let s_len = stop - start;
    let max_idx = s_len - 1;
    let empty =
      fun
      | first when first < 0 => {s, start, stop: start}
      | first when first > max_idx => {s, start: stop, stop}
      | first => {s, start: start + first, stop: start + first};
    if (len == 0) {
      empty(first);
    } else {
      let last /* index */ =
        switch (len) {
        | len when len == max_int => max_idx
        | len =>
          let last = first + len - 1;
          if (last > max_idx) {
            max_idx;
          } else {
            last;
          };
        };
      let first =
        if (first < 0) {
          0;
        } else {
          first;
        };
      if (first > max_idx || last < 0 || first > last) {
        empty(first);
      } else {
        {s, start: start + first, stop: start + last + 1 /* position */};
      };
    };
  };

let drop = (~rev=false, ~min=?, ~max=?, ~sat=?, t) =>
  (if (rev) {fst} else {snd}) @@ span(~rev, ~min?, ~max?, ~sat?, t);

let add_sub = (~no_empty, s, ~start, ~stop, acc) =>
  if (start == stop) {
    if (no_empty) {
      acc;
    } else {
      [{s, start, stop: start}, ...acc];
    };
  } else {
    [{s, start, stop}, ...acc];
  };

let fcuts =
    (
      ~no_empty,
      ~sep as {s: sep, start: sep_start, stop: sep_stop},
      {s, start, stop} as sub,
    ) => {
  let sep_len = sep_stop - sep_start;
  if (sep_len == 0) {
    invalid_arg("fcuts");
  } else {
    let s_len = stop - start;
    let max_sep_zidx = sep_len - 1;
    let max_s_idx = stop - sep_len;
    let rec check_sep = (sstart, i, k, acc) =>
      if (k > max_sep_zidx) {
        let new_start = i + sep_len;
        scan(
          new_start,
          new_start,
          add_sub(~no_empty, s, ~start=sstart, ~stop=i, acc),
        );
      } else if (s.[i + k] == sep.[sep_start + k]) {
        check_sep(sstart, i, k + 1, acc);
      } else {
        scan(sstart, i + 1, acc);
      }
    and scan = (sstart, i, acc) =>
      if (i > max_s_idx) {
        if (sstart == start) {
          if (no_empty && s_len == 0) {
            [];
          } else {
            [sub];
          };
        } else {
          List.rev(add_sub(~no_empty, s, ~start=sstart, ~stop, acc));
        };
      } else if (s.[i] == sep.[sep_start]) {
        check_sep(sstart, i, 1, acc);
      } else {
        scan(sstart, i + 1, acc);
      };
    scan(start, start, []);
  };
};

let rcuts =
    (
      ~no_empty,
      ~sep as {s: sep, start: sep_start, stop: sep_stop},
      {s, start, stop} as sub,
    ) => {
  let sep_len = sep_stop - sep_start;
  if (sep_len == 0) {
    invalid_arg("rcuts");
  } else {
    let s_len = stop - start;
    let max_sep_zidx = sep_len - 1;
    let max_s_idx = stop - 1;
    let rec check_sep = (sstop, i, k, acc) =>
      if (k > max_sep_zidx) {
        let start = i + sep_len;
        rscan(
          i,
          i - sep_len,
          add_sub(~no_empty, s, ~start, ~stop=sstop, acc),
        );
      } else if (s.[i + k] == sep.[sep_start + k]) {
        check_sep(sstop, i, k + 1, acc);
      } else {
        rscan(sstop, i - 1, acc);
      }
    and rscan = (sstop, i, acc) =>
      if (i < start) {
        if (sstop == stop) {
          if (no_empty && s_len == 0) {
            [];
          } else {
            [sub];
          };
        } else {
          add_sub(~no_empty, s, ~start, ~stop=sstop, acc);
        };
      } else if (s.[i] == sep.[sep_start]) {
        check_sep(sstop, i, 1, acc);
      } else {
        rscan(sstop, i - 1, acc);
      };
    rscan(stop, max_s_idx - max_sep_zidx, []);
  };
};

let cuts = (~rev=false, ~empty=true, ~sep, s) =>
  rev ? rcuts(~no_empty=!empty, ~sep, s) : fcuts(~no_empty=!empty, ~sep, s);
