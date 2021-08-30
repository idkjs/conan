/*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*/

open Result;

/* Julian day and proleptic Gregorian calendar date conversion.

   Formulae are from the calendar FAQ:
   http://www.tondering.dk/claus/cal/julperiod.php#formula

   These formulae work for positive Julian days. They represent
   Gegorian calendar BCE year `y` by `-(y-1)`, e.g. 2 BCE is -1, this
   follows the convention of ISO 8601.

   All timestamps in Ptime's [min;max] range are represented by
   positive Julian days and the formulae do not overflow on 32-bit
   platforms in this restricted range. */

let jd_to_date = jd => {
  let a = jd + 32044;
  let b = (4 * a + 3) / 146097;
  let c = a - 146097 * b / 4;
  let d = (4 * c + 3) / 1461;
  let e = c - 1461 * d / 4;
  let m = (5 * e + 2) / 153;
  let day = e - (153 * m + 2) / 5 + 1;
  let month = m + 3 - 12 * (m / 10);
  let year = 100 * b + d - 4800 + m / 10;
  (year, month, day);
};

let jd_of_date = ((year, month, day)) => {
  let a = (14 - month) / 12;
  let y = year + 4800 - a;
  let m = month + 12 * a - 3;
  day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
};

let jd_posix_epoch = 2_440_588; /* the Julian day of the POSIX epoch */

let jd_ptime_min = 1_721_060; /* the Julian day of Ptime.min */

let jd_ptime_max = 5_373_484; /* the Julian day of Ptime.max */

/* Picosecond precision POSIX timestamps and time span representation.

   POSIX timestamps and spans are represented by int * int64 pairs
   with the int64 in the range [0L;86_399_999_999_999_999L]. A pair
   [(d, ps)] denotes the POSIX picosecond duration [d] * 86_400e12 +
   [ps].

   For a timestamp this can be seen as a POSIX day count from the
   epoch paired with a picosecond precision POSIX time point in that
   day starting from 00:00:00.

   By definition with a negative [d] the [ps] duration brings us
   towards zero, *not* towards infinity:


         (d * 86_400e12) (d * 86_400e12 + ps)       0
     ... -----+-----------------+-------------------+--------- ...
              [---------------->|
                   ps

   [d] is largely sufficent to represent all the days in Ptime's
   [min;max] range on both 32-bit and 64-bit platforms. */

type t = (int, int64);

let ps_count_in_ps = 1L;

let ps_count_in_ns = 1_000L;

let ps_count_in_100ns = 100_000L;

let ps_count_in_us = 1_000_000L;

let ps_count_in_100us = 100_000_000L;

let ps_count_in_ms = 1_000_000_000L;

let ps_count_in_100ms = 100_000_000_000L;

let ps_count_in_s = 1_000_000_000_000L;

let ps_count_in_min = 60_000_000_000_000L;

let ps_count_in_hour = 3600_000_000_000_000L;

let ps_count_in_day = 86_400_000_000_000_000L;

let ps_day_max = 86_399_999_999_999_999L;

let day_min = jd_ptime_min - jd_posix_epoch;

let day_max = jd_ptime_max - jd_posix_epoch;

let epoch = (0, 0L); /* 1970-01-01 00:00:00 UTC */

let min = (day_min, 0L); /* 0000-01-01 00:00:00 UTC */

let max = (day_max, ps_day_max); /* 9999-12-31 23:59:59 UTC */

/* POSIX time spans */

type span = t;

module Span = {
  /* Arithmetic */

  let neg =
    fun
    | (d, 0L) => (- d, 0L)
    | (d, ps) => (- (d + 1), Int64.sub(ps_count_in_day, ps));

  let add = ((d0, ps0), (d1, ps1)) => {
    let d = d0 + d1;
    let ps = Int64.add(ps0, ps1);
    let ps_clamp = Int64.rem(ps, ps_count_in_day);
    let d = d + Int64.compare(ps, ps_clamp);
    (d, ps_clamp);
  };

  let sub = (s0, s1) => add(s0, neg(s1));

  let abs = ((d, _) as s) =>
    if (d < 0) {
      neg(s);
    } else {
      s;
    };

  /* POSIX time spans */

  type t = span;

  let zero = (0, 0L);

  let v = ((d, ps) as s) =>
    if (ps < 0L || ps > ps_day_max) {
      invalid_arg(
        Format.sprintf("illegal ptime time span: (%d,%Ld)", d, ps),
      );
    } else {
      s;
    };

  let of_d_ps = ((_, ps) as s) =>
    if (ps < 0L || ps > ps_day_max) {
      None;
    } else {
      Some(s);
    };

  let unsafe_of_d_ps = s => s;

  let unsafe_of_d_ps_option = s => s;

  let to_d_ps = s => s;

  let of_int_s = secs => {
    let d = Stdlib.abs(secs);
    let s = (d / 86_400, Int64.(mul(of_int(d mod 86_400), ps_count_in_s)));
    if (secs < 0) {
      neg(s);
    } else {
      s;
    };
  };

  let day_int_min = min_int / 86_400;

  let day_int_max = max_int / 86_400;

  let to_int_s = ((d, ps)) =>
    if (d < day_int_min || d > day_int_max) {
      None;
    } else {
      let days_s = d * 86_400;
      let day_s = Int64.(to_int(div(ps, ps_count_in_s))) /* always positive */;
      let secs = days_s + day_s;
      if (secs < days_s) /* positive overflow */ {
        None;
      } else {
        Some(secs);
      };
    };

  let min_int_float = float(min_int);

  let max_int_float = float(max_int);

  let of_float_s = secs =>
    if (secs != secs) {
      /* nan */
      None;
    } else {
      let days = floor(secs /. 86_400.);
      if (days < min_int_float || days > max_int_float) {
        None;
      } else {
        let rem_s = mod_float(secs, 86_400.);
        let rem_s =
          if (rem_s < 0.) {
            86_400. +. rem_s;
          } else {
            rem_s;
          };
        if (rem_s >= 86_400.) {
          Some((int_of_float(days) + 1, 0L));
        } else {
          let (frac_s, rem_s) = modf(rem_s);
          let rem_ps = Int64.(mul(of_float(rem_s), ps_count_in_s));
          let frac_ps = Int64.(of_float(frac_s *. 1e12));
          Some((int_of_float(days), Int64.add(rem_ps, frac_ps)));
        };
      };
    };

  let to_float_s = ((d, ps)) => {
    let days_s = float(d) *. 86_400.;
    let day_s = Int64.(to_float(div(ps, ps_count_in_s)));
    let day_rem_ps = Int64.(to_float(rem(ps, ps_count_in_s)));
    days_s +. day_s +. day_rem_ps *. 1e-12;
  };

  /* Predicates */

  let equal = ((d0, ps0), (d1, ps1)) =>
    (Stdlib.compare: (int, int) => int)(d0, d1) == 0
    && Int64.compare(ps0, ps1) == 0;

  let compare = ((d0, ps0), (d1, ps1)) => {
    let c = Stdlib.compare(d0, d1);
    if (c != 0) {
      c;
    } else {
      Stdlib.compare(ps0, ps1);
    };
  };

  /* Rounding */

  let round_div = (a, b) =>
    /* a >= 0 and b > 0 */
    if (a == 0L) {
      0L;
    } else {
      Int64.(div(add(a, div(b, 2L)), b));
    };

  let frac_div = [|
    1_000_000_000_000L,
    100_000_000_000L,
    10_000_000_000L,
    1_000_000_000L,
    100_000_000L,
    10_000_000L,
    1_000_000L,
    100_000L,
    10_000L,
    1_000L,
    100L,
    10L,
    1L,
  |];

  let round = (~frac_s as frac, (sign, _) as t) => {
    let frac =
      if (frac < 0) {
        0;
      } else if (frac > 12) {
        12;
      } else {
        frac;
      };
    let (d, ps) =
      if (sign < 0) {
        neg(t);
      } else {
        t;
      };
    let rps = Int64.mul(round_div(ps, frac_div[frac]), frac_div[frac]);
    let t =
      if (rps > ps_day_max) {
        (d + 1, 0L);
      } else {
        (d, rps);
      };
    if (sign < 0) {
      neg(t);
    } else {
      t;
    };
  };

  let truncate = (~frac_s as frac, (sign, _) as t) => {
    let frac =
      if (frac < 0) {
        0;
      } else if (frac > 12) {
        12;
      } else {
        frac;
      };
    let (d, ps) =
      if (sign < 0) {
        neg(t);
      } else {
        t;
      };
    let tps = Int64.(sub(ps, rem(ps, frac_div[frac])));
    if (sign < 0) {
      neg((d, tps));
    } else {
      (d, tps);
    };
  };

  /* Pretty printing */

  let dump = (ppf, (d, ps)) =>
    Format.fprintf(ppf, "@[<1>(%d,@,%Ld)@]", d, ps);

  /* Warning laborious code follows. Is there a better way ? */

  let divide_ps = (~carry, ps, hi, lo) => {
    let hi_d = Int64.(to_int(div(ps, hi)));
    let rem_ps = Int64.rem(ps, hi);
    let lo_d = Int64.to_int(round_div(rem_ps, lo));
    if (lo_d == carry) {
      (hi_d + 1, 0);
    } else {
      (hi_d, lo_d);
    };
  };

  let pp_y_d = (ppf, ~neg, d, ps) => {
    /* assert d >= 0 */
    let (y, rem_d) = {
      let max_d = max_int / 4;
      if (d > max_d) {
        /* d * 4 overflows */ (d / 365, d mod 365);
      } else {
        let y = d * 4 / 1461 /* / 365.25 */;
        (y, d - y * 1461 / 4);
      };
    };
    let days = rem_d + Int64.to_int(round_div(ps, ps_count_in_day));
    let (y, days) =
      if (days == 366) {
        (y + 1, 1);
      } else {
        (y, days);
      };
    let y =
      if (neg) {
        - y;
      } else {
        y;
      };
    Format.fprintf(ppf, "%dy", y);
    if (days != 0) {
      Format.fprintf(ppf, "%dd", days);
    };
    ();
  };

  let pp_d_h = (ppf, ~neg, d, ps) => {
    let (h, _) = divide_ps(~carry=1, ps, ps_count_in_hour, ps_count_in_hour);
    let (d, h) =
      if (h == 24) {
        (d + 1, 0);
      } else {
        (d, h);
      };
    if (d == 366) {
      Format.fprintf(ppf, "%dy1d", if (neg) {(-1)} else {1});
    } else if (d == 365 && h >= 6) {
      Format.fprintf(ppf, "%dy", if (neg) {(-1)} else {1});
    } else {
      let d =
        if (neg) {
          - d;
        } else {
          d;
        };
      Format.fprintf(ppf, "%dd", d);
      if (h != 0) {
        Format.fprintf(ppf, "%dh", h);
      };
      ();
    };
  };

  let pp_h_m = (ppf, ~neg, ps) => {
    let (h, m) = divide_ps(~carry=60, ps, ps_count_in_hour, ps_count_in_min);
    if (h == 24) {
      Format.fprintf(ppf, "%dd", if (neg) {(-1)} else {1});
    } else {
      let h =
        if (neg) {
          - h;
        } else {
          h;
        };
      Format.fprintf(ppf, "%dh", h);
      if (m != 0) {
        Format.fprintf(ppf, "%dmin", m);
      };
      ();
    };
  };

  let pp_m_s = (ppf, ~neg, ps) => {
    let (m, s) = divide_ps(~carry=60, ps, ps_count_in_min, ps_count_in_s);
    if (m == 60) {
      Format.fprintf(ppf, "%dh", if (neg) {(-1)} else {1});
    } else {
      let m =
        if (neg) {
          - m;
        } else {
          m;
        };
      Format.fprintf(ppf, "%dmin", m);
      if (s != 0) {
        Format.fprintf(ppf, "%ds", s);
      };
      ();
    };
  };

  let pp_s = (ppf, ~neg, ps) => {
    let (s, ms) = divide_ps(~carry=1000, ps, ps_count_in_s, ps_count_in_ms);
    if (s == 60) {
      Format.fprintf(ppf, "%dmin", if (neg) {(-1)} else {1});
    } else {
      let s =
        if (neg) {
          - s;
        } else {
          s;
        };
      if (ms != 0) {
        Format.fprintf(ppf, "%d.%ds", s, ms);
      } else {
        Format.fprintf(ppf, "%ds", s);
      };
    };
  };

  let pp_unit = (higher_str, hi, hi_str, frac_limit, lo, ppf, ~neg, ps) => {
    let pp_unit_integral = (ppf, ~neg, h) =>
      if (h == 1000) {
        Format.fprintf(ppf, "%d%s", if (neg) {(-1)} else {1}, higher_str);
      } else {
        Format.fprintf(
          ppf,
          "%d%s",
          if (neg) {
            - h;
          } else {
            h;
          },
          hi_str,
        );
      };
    if (ps < frac_limit) {
      let (h, l) = divide_ps(~carry=1000, ps, hi, lo);
      if (h >= 100 || l == 0) {
        pp_unit_integral(ppf, ~neg, h);
      } else {
        let h =
          if (neg) {
            - h;
          } else {
            h;
          };
        Format.fprintf(ppf, "%d.%d%s", h, l, hi_str);
      };
    } else {
      let (ms, _) = divide_ps(~carry=1, ps, hi, hi);
      pp_unit_integral(ppf, ~neg, ms);
    };
  };

  let pp_ms =
    pp_unit("s", ps_count_in_ms, "ms", ps_count_in_100ms, ps_count_in_us);

  let pp_us =
    pp_unit("ms", ps_count_in_us, "us", ps_count_in_100us, ps_count_in_ns);

  let pp_ns =
    pp_unit("us", ps_count_in_ns, "ns", ps_count_in_100ns, ps_count_in_ps);

  let pp_ps = (ppf, ~neg, ps) => {
    let ps = Int64.to_int(ps);
    Format.fprintf(
      ppf,
      "%dps",
      if (neg) {
        - ps;
      } else {
        ps;
      },
    );
  };

  let pp = (ppf, (sign, _) as s) => {
    let neg = sign < 0;
    switch (abs(s)) {
    | (0, ps) =>
      if (ps >= ps_count_in_hour) {
        pp_h_m(ppf, ~neg, ps);
      } else if (ps >= ps_count_in_min) {
        pp_m_s(ppf, ~neg, ps);
      } else if (ps >= ps_count_in_s) {
        pp_s(ppf, ~neg, ps);
      } else if (ps >= ps_count_in_ms) {
        pp_ms(ppf, ~neg, ps);
      } else if (ps >= ps_count_in_us) {
        pp_us(ppf, ~neg, ps);
      } else if (ps >= ps_count_in_ns) {
        pp_ns(ppf, ~neg, ps);
      } else {
        pp_ps(ppf, ~neg, ps);
      }
    | (d, ps) =>
      if (d > 365) {
        pp_y_d(ppf, ~neg, d, ps);
      } else {
        pp_d_h(ppf, ~neg, d, ps);
      }
    };
  };
};

/* POSIX timestamps */

let v = ((d, ps) as s) =>
  if (ps < 0L || ps > ps_day_max || d < day_min || d > day_max) {
    invalid_arg(Format.sprintf("illegal ptime timestamp: (%d,%Ld)", d, ps));
  } else {
    s;
  };

let unsafe_of_d_ps = s => s;

let of_span = ((d, _) as span) =>
  if (d < day_min || d > day_max) {
    None;
  } else {
    Some(span);
  };

let to_span = t => t;

let of_float_s = secs =>
  switch (Span.of_float_s(secs)) {
  | None => None
  | Some(d) => of_span(d)
  };

let to_float_s = Span.to_float_s;

let truncate = Span.truncate;

let frac_s = ((_, ps)) => (0, Int64.(rem(ps, ps_count_in_s)));

/* Predicates */

let equal = Span.equal;

let compare = Span.compare;

let is_earlier = (t, ~than) => compare(t, than) == (-1);

let is_later = (t, ~than) => compare(t, than) == 1;

/* POSIX arithmetic */

let add_span = (t, d) => of_span(Span.add(t, d));

let sub_span = (t, d) => of_span(Span.sub(t, d));

let diff = (t1, t0) => Span.sub(t1, t0);

/* Time zone offsets between local and UTC timelines */

type tz_offset_s = int;

/* Date-time conversion

   POSIX time counts seconds since 1970-01-01 00:00:00 UTC without
   counting leap seconds -- when a leap second occurs a POSIX second
   can be two SI seconds or zero SI second. Hence 86400 POSIX seconds
   always represent an UTC day and the translations below are accurate
   without having to refer to a leap seconds table. */

type date = (int, int, int);

type time = ((int, int, int), tz_offset_s);

let max_month_day = {
  /* max day number in a given year's month. */
  let is_leap_year = y => y mod 4 == 0 && (y mod 100 != 0 || y mod 400 == 0);
  let mlen = [|31, 28 /* or not */, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31|];
  (y, m) =>
    if (m == 2 && is_leap_year(y)) {
      29;
    } else {
      mlen[m - 1];
    };
};

let is_date_valid = ((y, m, d)) =>
  0 <= y
  && y <= 9999
  && 1 <= m
  && m <= 12
  && 1 <= d
  && d <= max_month_day(y, m);

let is_time_valid = (((hh, mm, ss), _)) =>
  0 <= hh && hh <= 23 && 0 <= mm && mm <= 59 && 0 <= ss && ss <= 60;

let of_date_time = ((date, ((hh, mm, ss), tz_offset_s) as t)) =>
  /* We first verify that the given date and time are Ptime-valid.
     Once this has been established we find find the number of Julian
     days since the epoch for the given proleptic Georgian calendar
     date. This gives us the POSIX day component of the timestamp. The
     remaining time fields are used to derive the picosecond precision
     time in that day compensated by the time zone offset. The final
     result is checked to be in Ptime's [min;max] range.

     By definition POSIX timestamps cannot represent leap seconds.
     With the code below any date-time with a seconds value of 60
     (leap second addition) is mapped to the POSIX timestamp that
     happens 1 second later which is what POSIX mktime would to. Any
     formally non-existing UTC date-time with a seconds value of 59
     (leap second subtraction) is mapped on the POSIX timestamp that
     represents this non existing instant. */
  if (!(is_date_valid(date) && is_time_valid(t))) {
    None;
  } else {
    let d = jd_of_date(date) - jd_posix_epoch;
    let hh_ps = Int64.(mul(of_int(hh), ps_count_in_hour));
    let mm_ps = Int64.(mul(of_int(mm), ps_count_in_min));
    let ss_ps = Int64.(mul(of_int(ss), ps_count_in_s));
    let ps = Int64.(add(hh_ps, add(mm_ps, ss_ps)));
    sub_span((d, ps), Span.of_int_s(tz_offset_s));
  };

let to_date_time = (~tz_offset_s=0, t) => {
  /* To render the timestamp in the given time zone offset we first
     express the timestamp in local time and then compute the date
     fields on that stamp as if it were UTC. If the local timestamp is
     not in [min;max] then its date fields cannot be valid according
     to the constraints guaranteed by Ptime and we fallback to UTC,
     i.e. a time zone offset of 0.

     We then apply the following algorithm whose description makes
     sense on a POSIX timestamp (i.e. UTC) but works equally well to
     render the date-time fields of a local timestamp.

     We first take take the POSIX day count [d] (equivalent by
     definition to an UTC day count) from the epoch, convert it to a
     Julian day and use this to get the proleptic Gregorian calendar
     date. The POSIX picoseconds [ps] in the day are are converted to
     a daytime according to to its various units.

     By definition no POSIX timestamp can represent a date-time with a
     seconds value of 60 (leap second addition) and thus the function
     will never return a date-time with such a value.  On the other
     hand it will return an inexisting UTC date-time with a seconds
     value of 59 whenever a leap second is subtracted since there is a
     POSIX timestamp that represents this instant. */
  let ((d, ps), tz_offset_s) =
    switch (add_span(t, Span.of_int_s(tz_offset_s))) {
    | None => (t, 0) /* fallback to UTC */
    | Some(local) => (local, tz_offset_s)
    };
  let jd = d + jd_posix_epoch;
  let date = jd_to_date(jd);
  let hh = Int64.(to_int(div(ps, ps_count_in_hour)));
  let hh_rem = Int64.rem(ps, ps_count_in_hour);
  let mm = Int64.(to_int(div(hh_rem, ps_count_in_min)));
  let mm_rem = Int64.rem(hh_rem, ps_count_in_min);
  let ss = Int64.(to_int(div(mm_rem, ps_count_in_s)));
  (date, ((hh, mm, ss), tz_offset_s));
};

let of_date = date => of_date_time((date, ((00, 00, 00), 0)));

let to_date = t => fst(to_date_time(~tz_offset_s=0, t));

let weekday = {
  let wday =
    /* Epoch was a thursday */
    [|`Thu, `Fri, `Sat, `Sun, `Mon, `Tue, `Wed|];
  (~tz_offset_s=0, t) => {
    let (d, _) = Span.add(t, Span.of_int_s(tz_offset_s));
    /* N.B. in contrast to [to_date_time] we don't care if we fall outside
       [min;max]. Even if it happens the result of the computation is still
       correct */
    let i = d mod 7;
    wday[if (i < 0) {
           7 + i;
         } else {
           i;
         }];
  };
};

/* RFC 3339 timestamp conversions */

/* RFC 3339 timestamp parser */

type error_range = (int, int);

type rfc3339_error = [
  | `Invalid_stamp
  | `Eoi
  | `Exp_chars(list(char))
  | `Trailing_input
];

let pp_rfc3339_error = ppf =>
  fun
  | `Invalid_stamp => Format.fprintf(ppf, "@[invalid@ time@ stamp@]")
  | `Eoi => Format.fprintf(ppf, "@[unexpected@ end@ of@ input@]")
  | `Trailing_input => Format.fprintf(ppf, "@[trailing@ input@]")
  | `Exp_chars(cs) => {
      let rec pp_chars = ppf => (
        fun
        | [c, ...cs] => {
            Format.fprintf(ppf, "@ %C", c);
            pp_chars(ppf, cs);
          }
        | [] => ()
      );
      Format.fprintf(ppf, "@[expected@ a@ character@ in:%a@]", pp_chars, cs);
    };

let rfc3339_error_to_msg =
  fun
  | Ok(_) as v => v
  | Error(`RFC3339((s, e), err)) =>
    Error(`Msg(Format.asprintf("%d-%d: %a", s, e, pp_rfc3339_error, err)));

exception RFC3339((int, int), rfc3339_error); /* Internal */

let error = (r, e) => raise([@implicit_arity] RFC3339(r, e));

let error_pos = (p, e) => error((p, p), e);

let error_exp_digit = p =>
  error_pos(
    p,
    `Exp_chars(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
  );

let is_digit =
  fun
  | '0' .. '9' => true
  | _ => false;

let parse_digits = (~count, pos, max, s) => {
  let stop = pos + count - 1;
  if (stop > max) {
    error_pos(max, `Eoi);
  } else {
    let rec loop = (k, acc) =>
      if (k > stop) {
        acc;
      } else if (is_digit(s.[k])) {
        loop(k + 1, acc * 10 + Char.code(s.[k]) - 0x30);
      } else {
        error_exp_digit(k);
      };
    loop(pos, 0);
  };
};

let parse_char = (c, pos, max, s) =>
  if (pos > max) {
    error_pos(max, `Eoi);
  } else if (s.[pos] == c) {
    ();
  } else {
    error_pos(pos, `Exp_chars([c]));
  };

let parse_dt_sep = (~strict, pos, max, s) => {
  let is_dt_sep =
    fun
    | 'T' => true
    | 't'
    | ' ' when !strict => true
    | _ => false;
  if (pos > max) {
    error_pos(max, `Eoi);
  } else if (is_dt_sep(s.[pos])) {
    ();
  } else {
    error_pos(
      pos,
      `Exp_chars(
        ['T']
        @ (
          if (strict) {
            [];
          } else {
            ['t', ' '];
          }
        ),
      ),
    );
  };
};

let decide_frac_or_tz = (~strict, pos, max, s) =>
  if (pos > max) {
    error_pos(max, `Eoi);
  } else {
    switch (s.[pos]) {
    | '.' => `Frac
    | '+'
    | '-'
    | 'Z' => `Tz
    | 'z' when !strict => `Tz
    | _ =>
      let chars =
        ['.', '+', '-', 'Z']
        @ (
          if (strict) {
            [];
          } else {
            ['z'];
          }
        );
      error_pos(pos, `Exp_chars(chars));
    };
  };

let parse_frac_ps = (pos, max, s) =>
  if (pos > max) {
    error_pos(max, `Eoi);
  } else if (!is_digit(s.[pos])) {
    error_exp_digit(pos);
  } else {
    let rec loop = (k, acc, pow) =>
      if (k > max) {
        error_pos(max, `Eoi);
      } else if (!is_digit(s.[k])) {
        (Some(acc), k);
      } else {
        let count = k - pos + 1;
        if (count > 12) {
          /* truncate */ loop(k + 1, acc, pow);
        } else {
          let pow = Int64.div(pow, 10L);
          let acc =
            Int64.(add(acc, mul(of_int(Char.code(s.[k]) - 0x30), pow)));
          loop(k + 1, acc, pow);
        };
      };
    loop(pos, 0L, ps_count_in_s);
  };

let parse_tz_s = (~strict, pos, max, s) => {
  let parse_tz_mag = (sign, pos) => {
    let hh_pos = pos;
    let mm_pos = hh_pos + 3;
    let hh = parse_digits(~count=2, hh_pos, max, s);
    parse_char(':', mm_pos - 1, max, s);
    let mm = parse_digits(~count=2, mm_pos, max, s);
    if (hh > 23) {
      error((hh_pos, hh_pos + 1), `Invalid_stamp);
    } else if (mm > 59) {
      error((mm_pos, mm_pos + 1), `Invalid_stamp);
    } else {
      let secs = hh * 3600 + mm * 60;
      let tz_s =
        secs == 0 && sign == (-1)
          ? None  /* -00:00 convention */
          : Some(sign * secs);
      (tz_s, mm_pos + 1);
    };
  };
  if (pos > max) {
    error_pos(max, `Eoi);
  } else {
    switch (s.[pos]) {
    | 'Z' => (Some(0), pos)
    | 'z' when !strict => (Some(0), pos)
    | '+' => parse_tz_mag(1, pos + 1)
    | '-' => parse_tz_mag(-1, pos + 1)
    | _ =>
      let chars =
        ['+', '-', 'Z']
        @ (
          if (strict) {
            [];
          } else {
            ['z'];
          }
        );
      error_pos(pos, `Exp_chars(chars));
    };
  };
};

let of_rfc3339 = (~strict=false, ~sub=false, ~start=0, s) =>
  try({
    let s_len = String.length(s);
    let max = s_len - 1;
    if (s_len == 0 || start < 0 || start > max) {
      error_pos(start, `Eoi);
    } else {
      let y_pos = start;
      let m_pos = y_pos + 5;
      let d_pos = m_pos + 3;
      let hh_pos = d_pos + 3;
      let mm_pos = hh_pos + 3;
      let ss_pos = mm_pos + 3;
      let decide_pos = ss_pos + 2;
      let y = parse_digits(~count=4, y_pos, max, s);
      parse_char('-', m_pos - 1, max, s);
      let m = parse_digits(~count=2, m_pos, max, s);
      parse_char('-', d_pos - 1, max, s);
      let d = parse_digits(~count=2, d_pos, max, s);
      parse_dt_sep(~strict, hh_pos - 1, max, s);
      let hh = parse_digits(~count=2, hh_pos, max, s);
      parse_char(':', mm_pos - 1, max, s);
      let mm = parse_digits(~count=2, mm_pos, max, s);
      parse_char(':', ss_pos - 1, max, s);
      let ss = parse_digits(~count=2, ss_pos, max, s);
      let (frac, tz_pos) =
        switch (decide_frac_or_tz(~strict, decide_pos, max, s)) {
        | `Frac => parse_frac_ps(decide_pos + 1, max, s)
        | `Tz => (None, decide_pos)
        };
      let (tz_s_opt, last_pos) = parse_tz_s(~strict, tz_pos, max, s);
      let tz_s =
        switch (tz_s_opt) {
        | None => 0
        | Some(s) => s
        };
      switch (of_date_time(((y, m, d), ((hh, mm, ss), tz_s)))) {
      | None => error((start, last_pos), `Invalid_stamp)
      | Some(t) =>
        let (t, _tz_s) =
          switch (frac) {
          | None
          | Some(0L) => (t, tz_s)
          | Some(frac) =>
            switch (add_span(t, (0, frac))) {
            | None => error((start, last_pos), `Invalid_stamp)
            | Some(t) => (t, tz_s)
            }
          };
        if (!sub && last_pos != max) {
          error_pos(last_pos + 1, `Trailing_input);
        } else {
          [@implicit_arity] Ok(t, tz_s_opt, last_pos - start + 1);
        };
      };
    };
  }) {
  | [@implicit_arity] RFC3339(r, e) => Error(`RFC3339((r, e)))
  };

/* RFC 3339 timestamp formatter */

let rfc3339_adjust_tz_offset = tz_offset_s => {
  /* The RFC 3339 time zone offset field is limited in expression to
     the bounds below with minute precision. If the requested time
     zone offset exceeds these bounds or is not an *integral* number
     of minutes we simply use UTC. An alternative would be to
     compensate the offset *and* the timestamp but it's more
     complicated to explain and maybe more surprising to the user. */
  let min = (-86340) /* -23h59 in secs */;
  let max = 86340 /* +23h59 in secs */;
  if (min <= tz_offset_s && tz_offset_s <= max && tz_offset_s mod 60 == 0) {
    (tz_offset_s, false);
  } else {
    (0 /* UTC */, true);
  };
};

let s_frac_of_ps = (frac, ps) =>
  Int64.(div(rem(ps, ps_count_in_s), Span.frac_div[frac]));

let to_rfc3339 =
    (~space=false, ~frac_s as frac=0, ~tz_offset_s=?, (_, ps) as t) => {
  let buf = Buffer.create(255);
  let (tz_offset_s, tz_unknown) =
    switch (tz_offset_s) {
    | Some(tz) => rfc3339_adjust_tz_offset(tz)
    | None => (0, true)
    };
  let ((y, m, d), ((hh, ss, mm), tz_offset_s)) =
    to_date_time(~tz_offset_s, t);
  let dt_sep = if (space) {' '} else {'T'};
  Printf.bprintf(
    buf,
    "%04d-%02d-%02d%c%02d:%02d:%02d",
    y,
    m,
    d,
    dt_sep,
    hh,
    ss,
    mm,
  );
  let frac =
    if (frac < 0) {
      0;
    } else if (frac > 12) {
      12;
    } else {
      frac;
    };
  if (frac != 0) {
    Printf.bprintf(buf, ".%0*Ld", frac, s_frac_of_ps(frac, ps));
  };
  if (tz_offset_s == 0 && !tz_unknown) {
    Printf.bprintf(buf, "Z");
  } else {
    let tz_sign =
      if (tz_offset_s < 0 || tz_unknown) {
        '-';
      } else {
        '+';
      };
    let tz_min = abs(tz_offset_s / 60);
    let tz_hh = tz_min / 60;
    let tz_mm = tz_min mod 60;
    Printf.bprintf(buf, "%c%02d:%02d", tz_sign, tz_hh, tz_mm);
  };
  Buffer.contents(buf);
};

let pp_rfc3339 = (~space=?, ~frac_s=?, ~tz_offset_s=?, (), ppf, t) =>
  Format.fprintf(ppf, "%s", to_rfc3339(~space?, ~frac_s?, ~tz_offset_s?, t));

/* Pretty printing */

let pp_human = (~frac_s as frac=0, ~tz_offset_s=?, (), ppf, (_, ps) as t) => {
  let (tz_offset_s, tz_unknown) =
    switch (tz_offset_s) {
    | Some(tz) => rfc3339_adjust_tz_offset(tz)
    | None => (0, true)
    };
  let ((y, m, d), ((hh, ss, mm), tz_offset_s)) =
    to_date_time(~tz_offset_s, t);
  Format.fprintf(ppf, "%04d-%02d-%02d %02d:%02d:%02d", y, m, d, hh, ss, mm);
  let frac =
    if (frac < 0) {
      0;
    } else if (frac > 12) {
      12;
    } else {
      frac;
    };
  if (frac != 0) {
    Format.fprintf(ppf, ".%0*Ld", frac, s_frac_of_ps(frac, ps));
  };
  let tz_sign =
    if (tz_offset_s < 0 || tz_unknown) {
      '-';
    } else {
      '+';
    };
  let tz_min = abs(tz_offset_s / 60);
  let tz_hh = tz_min / 60;
  let tz_mm = tz_min mod 60;
  Format.fprintf(ppf, " %c%02d:%02d", tz_sign, tz_hh, tz_mm);
  ();
};

let pp = pp_human(~tz_offset_s=0, ());

let dump = Span.dump;

/*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
