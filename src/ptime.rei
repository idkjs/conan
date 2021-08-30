/*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*/

/** POSIX time values.

    {!Ptime} has platform independent support for POSIX time. It provides a
    {{!t} type} to represent a well-defined range of POSIX timestamps with
    picosecond precision, conversion with {{!date_time} date-time values},
    conversion with {{!rfc3339} RFC 3339 timestamps} and {{!print} pretty
    printing} to a human-readable, locale-independent representation.

    {!Ptime_clock} provides access to a system POSIX clock and the system's
    current time zone offset. {!Ptime} is not a calendar library.

    Consult the {{!basics} basics} and a few {{!notes} notes and limitations}.

    {b References}

    - The Open Group.
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_15}
      The Open Group Base Specifications Issue 7, section 4.15 Seconds Since the
      Epoch}. 2013
    - G. Klyne et al. {{:http://tools.ietf.org/html/rfc3339}
      {e Date and Time on the Internet: Timestamps}}. RFC 3339, 2002.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%%} homepage}} */;

/** {1:timespans POSIX time spans} */;

/** The type for signed picosecond precision POSIX time spans. A value of this
    type represent the POSIX duration between two POSIX timestamps. */

type span;

/** POSIX time spans.

    {b WARNING.} A POSIX time span is not equal to an SI second based time span
    see the {{!basics} basics}. */

module Span: {
  /** {1:spans POSIX time spans} */;

  /** The type for signed, picosecond precision, POSIX time spans. */

  type t = span;

  /** [v s] is like {!of_d_ps}[s] but aise Invalid_argument if [s] is not in the
      right range. Use {!of_d_ps} to deal with untrusted input. */

  let v: ((int, int64)) => span;

  /** [zero] is the neutral element of {!add}. */

  let zero: span;

  /** [of_d_ps (d, ps)] is a span for the signed POSIX picosecond span [d] *
      86_400e12 + [ps]. [d] is a signed number of POSIX days and [ps] a number
      of picoseconds in the range \[[0];[86_399_999_999_999_999L]\]. [None] is
      returned if [ps] is not in the right range. */

  let of_d_ps: ((int, int64)) => option(span);



  let unsafe_of_d_ps: ((int, int64)) => span;

  let unsafe_of_d_ps_option: option((int, int64)) => option(span);



  /** [to_d_ps d] is the span [d] as a pair [(d, ps)] expressing the POSIX
      picosecond span [d] * 86_400e12 + [ps] with [ps] in the range
      \[[0];[86_399_999_999_999_999L]\] */

  let to_d_ps: span => (int, int64);

  /** [of_int_s secs] is a span from the signed integer POSIX second span
      [secs]. */

  let of_int_s: int => span;

  /** [to_int_s d] is the span [d] as a signed integer POSIX second span, if
      [int]'s range can represent it (note that this depends on
      {!Sys.word_size}). Subsecond precision numbers are truncated. */

  let to_int_s: span => option(int);

  /** [of_float_s secs] is a span from the signed floating point POSIX second
      span [d]. Subpicosecond precision numbers are truncated.

      [None] is returned if [secs] cannot be represented as a span. This occurs
      on {!Pervasives.nan} or if the duration in POSIX days cannot fit on an
      [int] (on 32-bit platforms this means the absolute magnitude of the
      duration is greater than ~2'941'758 years). */

  let of_float_s: float => option(span);

  /** [to_float_s s] is the span [d] as floating point POSIX seconds.

      {b Warning.} The magnitude of [s] may not be represented exactly by the
      floating point value. */

  let to_float_s: span => float;

  /** {1:predicates Predicates} */;

  /** [equal d d'] is [true] iff [d] and [d'] are the same time span. */

  let equal: (span, span) => bool;

  /** [compare d d'] is a total order on durations that is compatible with
      signed time span order. */

  let compare: (span, span) => int;

  /** {1:arith Arithmetic}

      {b Note.} The following functions rollover on overflows. */;

  /** [neg d] is the span [d] negated. */

  let neg: span => span;

  /** [add d d'] is [d] + [d']. */

  let add: (span, span) => span;

  /** [sub d d'] is [d] - [d']. */

  let sub: (span, span) => span;

  /** [abs d] is the absolute value of span [d]. */

  let abs: span => span;

  /** {1:rounding Rounding} */;

  /** [round ~frac_s t] is [t] rounded to the [frac_s] decimal fractional
      second. Ties are rounded away from zero. [frac_s] is clipped to the range
      \[[0];[12]\]. */

  let round: (~frac_s: int, span) => span;

  /** [truncate ~frac_s t] is [t] truncated to the [frac_s] decimal fractional
      second. [frac_s] is clipped to the range \[[0];[12]\]. */

  let truncate: (~frac_s: int, span) => span;

  /** {1:print Pretty printing} */;

  /** [pp ppf d] prints an unspecified, approximative, representation of [d] on
      [ppf].

      The representation is not fixed-width, depends on the magnitude of [d] and
      uses locale independent
      {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html} SI
      prefixes} on seconds and
      {{:http://www.bipm.org/en/publications/si-brochure/table6.html} accepted
      non-SI units}. Years are counted in Julian years (365.25 SI-accepted days)
      as {{:http://www.iau.org/publications/proceedings_rules/units/} defined}
      by the International Astronomical Union (IUA).

      The representation is approximative. In particular beyond 60 seconds it
      only keeps the two most significant time units and rounds towards the
      infinity. The latter means that case arising, it always {e over}
      approximates durations.

      {b Warning} Becomes unprecise (but does not overflow) if the absolute
      number of POSIX days in the time span is greater than [max_int / 4] (on
      32-bit platforms this is ~735'439 years) */

  let pp: (Format.formatter, span) => unit;

  /** [dump ppf s] prints an unspecified raw representation of [d] on [ppf]. */

  let dump: (Format.formatter, span) => unit;
};

/** {1:timestamps POSIX timestamps} */;

/** The type for picosecond precision POSIX timestamps in the range
    \[{!min};{!max}\]. Note that POSIX timestamps, and hence values of this
    type, are by definition always on the UTC timeline. */

type t;

/** [v s] is [of_span (Span.v s)] but raise Invalid_argument if [s] is not in
    the right range. Use {!Span.of_d_ps} and {!of_span} to deal with untrusted
    input. */

let v: ((int, int64)) => t;

/** [epoch] is 1970-01-01 00:00:00 UTC. */

let epoch: t;

/** [min] is 0000-01-01 00:00:00 UTC, the earliest timestamp representable by
    {!Ptime}. */

let min: t;

/** [max] is 9999-12-31 23:59:59.999999999999 UTC, the latest timestamp
    representable by {!Ptime}. */

let max: t;

/** [of_span d] is the POSIX time stamp that:

    - Happens at the POSIX span [d] {e after} {!epoch} if [d] is positive.
    - Happens at the POSIX span [d] {e before} {!epoch} if [d] is negative.

    [None] is returned if the timestamp is not in the range \[{!min};{!max}\]. */

let of_span: span => option(t);

/** [to_span t] is the signed POSIX span that happen between [t] and {!epoch}:

    - If the number is positive [t] happens {e after} {!epoch}.
    - If the number is negative [t] happens {e before} {!epoch}. */

let to_span: t => span;



let unsafe_of_d_ps: ((int, int64)) => t;



/** [of_float_s d] is like {!of_span} but with [d] as a floating point second
    POSIX span [d]. This function is compatible with the result of
    {!Unix.gettimeofday}. Decimal fractional seconds beyond [1e-12] are
    truncated. */

let of_float_s: float => option(t);

/** [to_float_s t] is like {!to_span} but returns a floating point second POSIX
    span.

    {b Warning.} Due to floating point inaccuracies do not expect the function
    to round trip with {!of_float_s}; especially near {!Ptime.min} and
    {!Ptime.max}. */

let to_float_s: t => float;

/** [truncate ~frac_s t] is [t] truncated to the [frac_s] decimal fractional
    second. Effectively this reduces precision without rounding, the timestamp
    remains in the second it is in. [frac_s] is clipped to the range
    \[[0];[12]\]. */

let truncate: (~frac_s: int, t) => t;

/** [frac_s t] is the (positive) fractional second duration in [t]. */

let frac_s: t => span;

/** {1:predicates Predicates} */;

/** [equal t t'] is [true] iff [t] and [t'] are the same timestamps. */

let equal: (t, t) => bool;

/** [compare t t'] is a total order on timestamps that is compatible with
    timeline order. */

let compare: (t, t) => int;

/** [is_earlier t ~than] is [true] iff [compare t than = -1]. */

let is_earlier: (t, ~than: t) => bool;

/** [is_later t than] is [true] iff [compare t than = 1]. */

let is_later: (t, ~than: t) => bool;

/** {1:posix_arithmetic POSIX arithmetic}

    {b WARNING.} A POSIX time span is not equal to an SI second based time span,
    see the {{!basics} basics}. Do not use these functions to perform calendar
    arithmetic or measure wall-clock durations, you will fail. */;

/** [add_span t d] is timestamp [t + d], that is [t] with the signed POSIX span
    [d] added. [None] is returned if the result is not in the range
    \[{!min};{!max}\]. */

let add_span: (t, span) => option(t);

/** [sub_span t d] is the timestamp [t - d], that is [t] with the signed POSIX
    span [d] subtracted. [None] is returned if the result is not in the range
    \[{!min};{!max}\]. */

let sub_span: (t, span) => option(t);

/** [diff t t'] is the signed POSIX span [t - t'] that happens between the
    timestamps [t] and [t']. */

let diff: (t, t) => span;

/** {1:tz_offset Time zone offsets between local and UTC timelines} */;

/** The type for time zone offsets between local and UTC timelines in seconds.
    This is the signed difference in seconds between the local timeline and the
    UTC timeline:

    {[ tz_offset_s = local - UTC ]}
    - A value of [-3600] means that the local timeline is sixty minutes
      {e behind} the UTC timeline.
    - A value of [3600] means that the local timeline is sixty minutes {e ahead}
      the UTC timeline. */

type tz_offset_s = int;

/** {1:date_time Date-time value conversions}

    A {e date-time} represents a point on the UTC timeline by pairing a date in
    the proleptic Gregorian calendar and a second precision daytime in a local
    timeline with stated relationship to the UTC timeline. */;

/** The type for big-endian proleptic Gregorian dates. A triple [(y, m, d)]
    with:

    - [y] the year from [0] to [9999]. [0] denotes -1 BCE (this follows the
      {{:http://www.iso.org/iso/home/standards/iso8601.htm} ISO 8601}
      convention).
    - [m] is the month from [1] to [12]
    - [d] is the day from [1] to [28], [29], [30] or [31] depending on [m] and
      [y]

    A date is said to be {e valid} iff the values [(y, m, d)] are in the range
    mentioned above and represent an existing date in the proleptic Gregorian
    calendar. */

type date = (int, int, int);

/** The type for daytimes on a local timeline. Pairs a triple [(hh, mm, ss)]
    denoting the time on the local timeline and a [tz_offset] stating the
    {{!tz_offset_s} relationship} of the local timeline to the UTC timeline.

    The [(hh, mm, ss)] components are understood and constrainted as follows:

    - [hh] is the hour from [0] to [23].
    - [mm] is the minute from [0] to [59].
    - [ss] is the seconds from [0] to [60]. [60] may happen whenever a leap
      second is added.

    A [time] value is said to be {e valid} iff the values [(hh, mm, ss)] are in
    the ranges mentioned above. */

type time = ((int, int, int), tz_offset_s);

/** [of_date_time dt] is the POSIX timestamp corresponding to date-time [dt] or
    [None] if [dt] has an {{!date} invalid date}, {{!time} invalid time} or the
    date-time is not in the range \[{!min};{!max}\].

    {b Leap seconds.} Any date-time with a seconds value of [60], hence
    representing a leap second addition, is mapped to the date-time that happens
    1 second later. Any date-time with a seconds value of [59] is mapped to the
    POSIX timestamp that represents this instant, if a leap second was
    subtracted at that point, this is the POSIX timestamp that represents this
    inexisting instant. See the {{!basics} basics}. */

let of_date_time: ((date, time)) => option(t);

/** [to_date_time ~tz_offset_s t] is the date-time of the timestamp [t].

    [tz_offset_s] hints the time zone offset used for the resulting daytime
    component (defaults to [0], i.e. UTC). The offset is not honoured and
    fallbacks to [0] in case the resulting date-time rendering of the timestamp
    would yield an {{!date} invalid date}. This means that you should always
    interpret the resulting time component with the time zone offset it is
    paired with in the result and not assume it will be the one you gave to the
    function. Note that for real-world time zone offsets the fallback to [0]
    will only happen around {!Ptime.min} and {!Ptime.max}. Formally the fallback
    occurs whenever [add_span t (Span.of_int_s tz_offset_s)] is [None].

    {b Leap seconds.} No POSIX timestamp can represent a date-time with a leap
    second added, hence this function will never return a date-time with a [60]
    seconds value. This function does return inexisting UTC date-times with [59]
    seconds whenever a leap second is subtracted since POSIX timestamps do
    represent them. See the {{!basics} basics}.

    {b Subsecond precision.} POSIX timestamps with subsecond precision are
    floored, i.e. the date-time always has the second mentioned in the
    timestamp. */

let to_date_time: (~tz_offset_s: tz_offset_s=?, t) => (date, time);

/** [of_date d] is [of_date_time (d, ((00, 00, 00), 0 (* UTC *)))]. */

let of_date: date => option(t);

/** [to_date t] is [fst (to_date_time t)]. */

let to_date: t => date;

/** [weekday ~tz_offset_s t] is the day in the 7-day week of timestamp [t]
    expressed in the time zone offset [ts_offset_s] (defaults to [0]).

    This can be used with the time zone offset result of {!to_date_time} to
    convert timestamps to denormalized timestamp formats. */

let weekday:
  (~tz_offset_s: tz_offset_s=?, t) =>
  [ | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat | `Sun];

/** {1:rfc3339 RFC 3339 timestamp conversions} */;

/** The type for error ranges, starting and ending position. */

type error_range = (int, int);

/** The type for RFC 3339 timestamp parsing errors. [`Invalid_stamp] means that
    either the time stamp is not in the range \[{!min};{!max}\], or the date is
    invalid, or one of the fields is not in the right range. */

type rfc3339_error = [
  | `Invalid_stamp
  | `Eoi
  | `Exp_chars(list(char))
  | `Trailing_input
];

/** [pp_rfc3339_error ppf e] prints an unspecified representation of [e] on
    [ppf]. */

let pp_rfc3339_error: (Format.formatter, rfc3339_error) => unit;

/** [rfc3339_error_to_msg r] converts RFC 3339 parse errors to error messages. */

let rfc3339_error_to_msg:
  result('a, [ | `RFC3339(error_range, rfc3339_error)]) =>
  result('a, [> | `Msg(string)]);

/** [of_rfc3339 ~strict ~sub ~start s] parses an RFC 3339
    {{:https://tools.ietf.org/html/rfc3339#section-5.6} [date-time]} starting at
    [start] (defaults to [0]) in [s] to a triple [(t, tz, count)] with:

    - [t] the POSIX timestamp (hence on the UTC timeline).
    - [tz], the optional {{!tz_offset_s} time zone offset} found in the
      timestamp. [None] is returned iff the date-time satisfies the
      {{:https://tools.ietf.org/html/rfc3339#section-4.3} unknown local offset
      convention}.
    - [count] the number of bytes read starting at [start] to parse the
      timestamp. If [sub] is [false] (default) this is always
      [String.length s - start] and [Error `Trailing_input] is returned if there
      are still bytes in [s] after the date-time was parsed. Use [~sub:true] for
      allowing trailing input to exist.

    If [strict] is [true] (defaults to [false]) the parsing function errors on
    timestamps with lowercase ['T'] or ['Z'] characters or space separated date
    and times.

    {b Notes and limitations.}

    - If [start] is not an index of [s], [Error ((start, start), `Eoi)] is
      returned.
    - RFC 3339 allows a few degenerate (I say) timestamps with non-zero time
      zone offsets to be parsed at the boundaries that correspond to timestamps
      that cannot be expressed in UTC in RFC 3339 itself (e.g.
      [0000-01-01T00:00:00+00:01]). The function errors on these timestamps with
      [`Invalid_stamp] as they cannot be represented in the range
      \[{!min};{!max}\].
    - Leap seconds are allowed on any date-time and handled as in
      {!of_date_time}
    - Fractional parts beyond the picosecond ([1e-12]) are truncated. */

let of_rfc3339:
  (~strict: bool=?, ~sub: bool=?, ~start: int=?, string) =>
  result(
    (t, option(tz_offset_s), int),
    [> | `RFC3339(error_range, rfc3339_error)],
  );

/** [to_rfc3339_tz ~space ~frac_s ~tz_offset_s t] formats the timestamp [t]
    according to a RFC 3339 {{:https://tools.ietf.org/html/rfc3339#section-5.6}
    [date-time]} production with:

    - [tz_offset_s] hints the time zone offset to use, use [0] for UTC. The hint
      is ignored in the following cases: if [tz_offset_s] is not an integral
      number of minutes and its magnitude not in the range permitted by the
      standard, if [add_span t (Span.of_int_s tz_offset_s)] is [None] (the
      resulting timestamp rendering would not be RFC 3339 compliant). If either
      the hint is ignored or [tz_offset_s] is unspecified then the
      {{:https://tools.ietf.org/html/rfc3339#section-4.3} unknown local offset
      convention} is used to render the time zone component.
    - [frac_s], clipped to the range \[[0];[12]\] specifies that exactly
      [frac_s] decimal digits of the fractional second of [t] are rendered
      (defaults to [0]).
    - [space] if [true] the date and time separator is a space rather than a
      ['T'] (not recommended but may be allowed by the protocol you are dealing
      with, defaults to [false]). */

let to_rfc3339:
  (~space: bool=?, ~frac_s: int=?, ~tz_offset_s: tz_offset_s=?, t) => string;

/** [pp_rfc3339 ?space ?frac_s ?tz_offset_s () ppf t] is
    [Format.fprintf ppf "%s" (to_rfc3339 ?space ?frac_s ?tz_offset_s t)]. */

let pp_rfc3339:
  (
    ~space: bool=?,
    ~frac_s: int=?,
    ~tz_offset_s: tz_offset_s=?,
    unit,
    Format.formatter,
    t
  ) =>
  unit;

/** {1:print Pretty printing} */;

/** [pp_human ~frac_s ~tz_offset_s () ppf t] prints an unspecified, human
    readable, locale-independent, representation of [t] with:

    - [tz_offset_s] hints the time zone offset to use. The hint is ignored in
      the following cases: if [tz_offset_s] is not an integral number of minutes
      and its magnitude not in the range permitted by the standard, if
      [add_span t (Span.of_int_s tz_offset_s)] is [None]. If either the hint is
      ignored or [tz_offset_s] is unspecified then RFC 3339's
      {{:https://tools.ietf.org/html/rfc3339#section-4.3} unknown local offset
      convention} is used to render the time zone component.
    - [frac_s] clipped to the range \[[0];[12]\] specifies that exactly [frac_s]
      decimal digits of the fractional second of [t] are rendered (defaults to
      [0]).

    {b Note.} The output of this function is similar to but {b not} compliant
    with RFC 3339, it should only be used for presentation, not as a
    serialization format. */

let pp_human:
  (~frac_s: int=?, ~tz_offset_s: tz_offset_s=?, unit, Format.formatter, t) =>
  unit;

/** [pp] is [pp_human ~tz_offset_s:0]. */

let pp: (Format.formatter, t) => unit;

/** [dump ppf t] prints an unspecified raw representation of [t] on [ppf]. */

let dump: (Format.formatter, t) => unit;

/** {1:basics Basics}

    POSIX time counts {{!posix_seconds} POSIX seconds} since the epoch
    1970-01-01 00:00:00 UTC. As such a POSIX timestamp is {b always} on the UTC
    timeline.

    POSIX time doesn't count leap seconds, so by definition it cannot represent
    them. One way of viewing this is that whenever a leap second is added a
    POSIX second lasts two SI seconds and whenever a leap second is subtracted a
    POSIX second lasts zero SI second.

    {!Ptime} does not provide any mean to convert the duration between two POSIX
    timestamps to SI seconds. The reason is that in order to accurately find
    this number, a {{:http://www.ietf.org/timezones/data/leap-seconds.list} leap
    second table} is needed. However since this table may change every six
    months, {!Ptime} decides not to include it so as not to potentially become
    incorrect every six months.

    This decision has the following implications. First it should be realised
    that the durations mentioned by the {!add_span}, {!sub_span} and {!diff}
    functions are expressed in {e POSIX seconds} which may represent zero, one,
    or two SI seconds. For example if we add 1 second with {!add_span} to the
    POSIX timestamp for 1998-12-31 23:59:59 UTC, what we get is the timestamp
    for 1999-01-01 00:00:00 UTC:

    {[
      let get = function None -> assert false | Some v -> v

      let utc d t = get @@ Ptime.of_date_time (d, (t, 0))

      let t0 = utc (1998, 12, 31) (23, 59, 59)

      let t1 = utc (1999, 01, 01) (00, 00, 00)

      let one_s = Ptime.Span.of_int_s 1

      let () = assert (Ptime.equal (get @@ Ptime.add_span t0 one_s) t1)
    ]}

    However since the leap second 1998-12-31 23:59:60 UTC exists, {e two} actual
    SI seconds elapsed between [t0] and [t1]. Now if we use {!diff} to find the
    POSIX duration that elapsed between [t0] and [t1] we get one POSIX second:

    {[ let () = assert (Ptime.Span.equal (Ptime.diff t1 t0) one_s) ]}

    But still, two SI seconds elapsed between these two points in time. Note
    also that no value of type {!t} can represent the UTC timetamp 1998-12-31
    23:59:60 and hence {!Ptime.to_date_time} will never return a date-time with
    a seconds value of [60]. In fact both 1998-12-31 23:59:60 UTC and 1999-01-01
    00:00:00 UTC are represented by the same timestamp:

    {[
      let t2 = utc (1998, 12, 31) (23, 59, 60)

      let () = assert (Ptime.equal t1 t2)
    ]}

    This is true of any added leap second, we map it on the first second of the
    next minute, thus matching the behaviour of POSIX's
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/mktime.html}
    mktime} function.

    If a leap second is subtracted on a day the following occurs – 2015, as of
    writing this never happened. Let YYYY-06-30 23:59:58 be the instant a leap
    second is subtracted, this means that the next UTC date-time, one SI second
    later, is YYYY-07-01 00:00:00. However if we diff the two instants:

    {[
      let y = 9999 (* hypothetical year were this happens *)

      let t0 = utc (y, 06, 30) (23, 59, 58)

      let t1 = utc (y, 07, 01) (00, 00, 00)

      let two_s = Ptime.Span.of_int_s 2

      let () = assert (Ptime.Span.equal (Ptime.diff t1 t0) two_s)
    ]}

    We get two POSIX seconds, but only one SI second elapsed between these two
    points in time. It should also be noted that POSIX time will represent a
    point that never existed in time namely YYYY-06-30 23:59:59, the POSIX
    second with 0 SI second duration and that {!Ptime.to_date_time} will return
    a date-time value for this timestamp even though it never existed:

    {[
      let t2 = utc (y, 06, 30) (23, 59, 59)

      let () = assert (Ptime.equal (get @@ Ptime.add_span t0 one_s) t2)
    ]}

    {1:notes Notes and limitations}

    The following points should be taken into account

    {ul
     {- {!Ptime} is not a calendar library and will never be. }
     {- {!Ptime}
        can
        only
        represent
        picosecond
        precision
        timestamps
        in
        the
        range
        \[{!Ptime.min};{!Ptime.max}\].
        It
        is
        however
        able
        to
        convert
        {e any}
        of
        these
        timestamps
        to
        a
        valid
        date-time
        or
        RFC
        3339
        timestamp.
     }
     {- POSIX
        time
        in
        general
        is
        ill-suited
        to
        measure
        wall-clock
        time
        spans
        for
        the
        following
        reasons.

        - POSIX time counts time in POSIX seconds. POSIX seconds can represent
          2, 1 or 0 SI seconds. [Ptime] offers no mechanism to determine the SI
          duration between two timestamps, see the {{!basics} basics}.
        - The POSIX timestamps returned by your platform are not monotonic: they
          are subject to operating system time adjustements and can even go back
          in time. If you need to measure time spans in a single program run use
          a monotonic time source (e.g. {!Mtime}).
     }
    } */;

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
