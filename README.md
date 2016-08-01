# Overview
Jam (from an Indonesian word for "clock" or "time") is a time parsing
and processing library written to allow Riak's timeseries database to
use fine-grained date and time types.

Jam's initial release includes an ISO 8601 parser/formatter, but the
architecture allows for any syntax to be supported by adding a new
module that understands Jam's date/time structures.

There are several phases of processing available to allow an
application to massage the data for more sophisticated purposes than
merely translating strings to UNIX epoch time.

## Phases

### Parsing

Currently Jam supports (most of) ISO 8601 but other formats are
anticipated.

Yet to be supported:

* Date/time intervals
* Week dates

See `jam_iso8601:parse/{1,2}`.

### Compilation

The compilation step (`jam:compile/{1,2}`) converts the strings
captured by parsing into a (possibly valid) date and/or time. The
resulting tuples will not be the same as Erlang's date/time tuples
because this library permits fractional seconds, "incomplete" dates
and times, and time zones.

### Validation

Validation is entirely optional. It is possible to extensively
manipulate invalid date/time values, and developers are advised to
validate at whatever point in the pipeline seems reasonable (but no
earlier than compilation).

The compilation step only exercises as much knowledge about "real"
times and dates as is necessary (e.g., knowing what years are leap
years to properly interpret ordinal dates). A time such as "25:15" is
a possible outcome of the parsing and compilation steps, so validation
functions `is_valid/{1,2}` are supplied.

Also see **Expansion** below for a discussion of incompleteness. An
incomplete date or time can still be valid.

### Normalization

There are two edge case time values which may arise, and which are
corrected for using `jam:normalize/1`.

The first, permitted by ISO 8601, is "24:00". This is *not* the same
as "00:00", at least when also attached to a date. "2016-06-15 24:00"
is the same as "2016-06-16 00:00" and normalization will convert the
former into the latter.

The second unusual time value: seconds > 59.

Occasionally [leap seconds](https://en.wikipedia.org/wiki/Leap_second)
will be added at the end of a day. UNIX/POSIX time will silently
["absorb"](https://en.wikipedia.org/wiki/Unix_time#Leap_seconds) that
into the first second of the following day.

Thus, a second value of `60` is considered valid by the validation
functions and is converted to `00` seconds in the following minute
during normalization.

### Expansion

ISO 8601 allows for incomplete date/time strings.

We want a complete date/time for some conversions, so expansion of an
incomplete date/time record via `jam:expand/2` is an optional step in
the processing timeline.

The values added to expand an incomplete record are the lower bounds
for the appropriate fields, so 1 for month or day, 0 for any time
field.

`jam:is_complete/1` can be used to test a datetime structure (compiled
or not) for completeness. Use `jam:is_complete_date/1` or
`jam:is_complete_time/1` for dates and times outside a datetime
structure.

It may be useful to adjust a compiled, incomplete date/time via
`jam:increment/2`. This identifies the least significant populated
value and adds an integer value. One example is strictly greater-than
comparisons in Riak's timeseries support.

### Translation

Dates and times can be converted to alternative time zones
(`jam:convert_tz/2`), to ISO 8601 (and, in the future, other formats)
(`jam_iso8601:to_string/{1,2}`), to Erlang's date/time tuples (see
`jam_erlang`), or to integers (UNIX epoch with support for sub-second
values via `jam:to_epoch/{1,2}`).

`jam:round_fractional_seconds/1` is recommended before converting to
Erlang tuples.

Jam's internal records are not intended to be manipulated or examined
by code outside the library, thus doing so places your code at risk of
breaking with future releases.

UTC epoch integers can also be used to create jam datetime records:
see `jam:from_epoch/{1,2}`.

## Illustrated usage


```erlang
1> jam_iso8601:parse("1985-04-23 13:15:57Z").
{parsed_datetime,{parsed_calendar,"1985","04","23"},
                 {parsed_time,"13","15","57",undefined,undefined,
                              {parsed_timezone,"Z",undefined,undefined}}}
2> jam:compile(v(-1)).
{datetime,{date,1985,4,23},
          {time,13,15,57,undefined,undefined,{timezone,"Z",0,0}}}
3> jam:normalize(v(-1)).
{datetime,{date,1985,4,23},
          {time,13,15,57,undefined,undefined,{timezone,"Z",0,0}}}
4> jam:to_epoch(v(-1)).
483110157
5> jam_iso8601:to_string(v(-2)).
"1985-04-23T13:15:57Z"
6> jam_iso8601:to_string(v(-3), [{format, basic}]).
"19850423T131557Z"
7> jam_iso8601:to_string(v(-4), [{format, basic}, {z, false}]).
"19850423T131557+0000"
```

For a more sophisticated example, we handle a leap second with a
fractional component and time zone conversion.

Note that in order to support passing times without dates as
arguments, `jam:offset_normalize/1`,
`jam:offset_round_fractional_seconds/1` and `jam:offset_convert_tz/2`
return a two-tuple, with the first value an integer expressing whether
or not a date adjustment resulted. The non-`offset_` version of each
function drops the date adjustment in favor of a single return value, and we'll use those here.

```erlang
1> DT1 = jam:normalize(jam:compile(jam_iso8601:parse("20150630T23:59:60.738Z"))).
{datetime,{date,2015,7,1},
          {time,0,0,0,
                {fraction,0.738,3},
                undefined,
                {timezone,"Z",0,0}}}
2> DT2 = jam:round_fractional_seconds(DT1).
{datetime,{date,2015,7,1},
          {time,0,0,1,undefined,undefined,{timezone,"Z",0,0}}}
3> DT3 = jam:convert_tz(DT2, "-07:30").
{datetime,{date,2015,6,30},
          {time,16,30,1,undefined,undefined,
                {timezone,"-07:30",7,30}}}
4> jam_iso8601:to_string(DT3).
"2015-06-30T16:30:01-07:30"
```

Here's an example of expanding an incomplete date. Note that if we specify a target for our expansion of less than a day, the original date record becomes a datetime so we can expand the time as well.

```erlang
2> DT = jam:compile(jam_iso8601:parse("2016")).
#date{year = 2016,month = undefined,day = undefined}
3> jam:expand(DT, day).
#date{year = 2016,month = 1,day = 1}
4> jam:expand(DT, minute).
#datetime{date = #date{year = 2016,month = 1,day = 1},
          time = #time{hour = 0,minute = 0,second = undefined,
                       fraction = undefined,subsecond = undefined,
                       timezone = undefined}}
```

Dates or times can be managed independently. This snippet also
illustrates compiling the ISO 8601 regular expressions for better
parsing performance.

```erlang
1> REs = jam_iso8601:init().
[{time,{re_pattern,14,0,
                   <<69,82,67,80,152,1,0,0,16,0,0,0,1,0,0,0,14,0,3,0,0,0,
                     ...>>}},
 {ordinal_date,{re_pattern,3,0,
                           <<69,82,67,80,125,0,0,0,16,0,0,0,1,0,0,0,3,0,0,0,0,...>>}},
 {calendar_date,{re_pattern,6,0,
                            <<69,82,67,80,172,0,0,0,16,0,0,0,1,0,0,0,6,0,3,0,...>>}},
 {week_date,{re_pattern,7,0,
                        <<69,82,67,80,174,0,0,0,16,0,0,0,5,0,0,0,7,0,3,...>>}},
 {ordinal_datetime,{re_pattern,17,0,
                               <<69,82,67,80,15,2,0,0,16,0,0,0,1,0,0,0,17,0,...>>}},
 {week_datetime,{re_pattern,21,0,
                            <<69,82,67,80,69,2,0,0,16,0,0,0,5,0,0,0,21,...>>}},
 {calendar_datetime,{re_pattern,20,0,
                                <<69,82,67,80,62,2,0,0,16,0,0,0,1,0,0,0,...>>}},
 {timezone,{re_pattern,5,0,
                       <<69,82,67,80,177,0,0,0,16,0,0,0,1,0,0,...>>}}]
2> Time = jam_iso8601:parse(REs, "T14:15:16-05").
{parsed_time,"14","15","16",undefined,undefined,
             {parsed_timezone,"-05","-05",undefined}}
```

Jam also allows Erlang datetime tuples to be converted to strings.

The `iso8601_tests` module further illustrates usage of the `jam`
library.

## Joe's Abstract Machine

The term "JAM" in Erlang originally referred to Joe Armstrong's
prolog-based Erlang VM. The name collision is purely coincidental, but
convenient in that there appear to be no existing Erlang libraries
named `jam` (perhaps as a consequence).
