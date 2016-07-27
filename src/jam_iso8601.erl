%% -------------------------------------------------------------------
%%
%% jam_iso8601: Support ISO 8601 date/time strings.
%%
%% Copyright (c) 2016 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(jam_iso8601).

-export([init/0]).

-export([parse/1, parse/2,
         parse_time/1, parse_time/2,
         parse_date/1, parse_date/2,
         parse_datetime/1, parse_datetime/2,
         parse_tz/1, parse_tz/2,
         to_string/1, to_string/2]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jam_iso8601.hrl").
-include("jam_internal.hrl").

-type parse_error() :: 'week_dates_unsupported' | 'undefined'.

%% There is limited flexibility allowed in parsing, to comply with ISO
%% 8601: The time designator "T" used to separate date and time can be
%% a single space instead.

%% A 4 digit string with no separators is potentially ambiguous: YYYY
%% or HHMM? (YYYYMM is explicitly forbidden by the spec: a separator
%% must be included).
%%
%% Another ambiguity: 2012-06 could be YYYY-MM or HHMM-TZ. We will
%% prefer YYYY-MM.
%%
%% To address those ambiguities (and perhaps others I have failed to
%% notice) the spec mandates that "T" be used in front of a local time
%% in any context where it could be ambiguous. The "T" is optional
%% and supported in other contexts.
%%
%% And, of course, a time zone specification (including "Z") at the
%% end of a string will clearly indicate that it is a time and not a
%% date.

%% If a string does not match the standard's requirements, `undefined'
%% will be returned.

re_list() ->
    [
     {time, ?ANCHOR(?TTIME)},
     {ordinal_date, ?ANCHOR(?ORDINAL_DATE)},
     {calendar_date, ?ANCHOR(?CALENDAR_DATE)},
     {week_date, ?ANCHOR(?WEEK_DATE)},
     {ordinal_datetime, ?ANCHOR(?ORDINAL_TIME("[ T]"))},
     {week_datetime, ?ANCHOR(?WEEK_TIME("[ T]"))},
     {calendar_datetime, ?ANCHOR(?CALENDAR_TIME("[ T]"))},
     {timezone, ?ANCHOR(?TIMEZONE)}
    ].

%% Long-running processes can compile all of the regular expressions
%% first to save time on parsing
init() ->
    lists:map(fun({Label, RE}) ->
                      {ok, Compiled} = re:compile(RE),
                      {Label, Compiled}
              end, re_list()).

%% parse/{1,2} is generally less efficient than the explicit alternatives,
%% but a leading "T" means we can restrict ourselves to looking at
%% time parsing
-spec parse(string()) -> parsed_time() | parsed_date() | parsed_datetime() | parse_error().
parse(String) ->
    parse(re_list(), String).

-spec parse(list(), string()) -> parsed_time() | parsed_date() | parsed_datetime() | parse_error().
parse(REs, [$T|Rest]) ->
    parse_time(REs, Rest);
parse(REs, String) ->
    %% Time parsing is probably fastest, but any ambiguous formats
    %% should be interpreted as dates, not times
    try_each([fun(S) -> parse_datetime(REs, S) end,
              fun(S) -> parse_date(REs, S) end,
              fun(S) -> parse_time(REs, S) end], String).

try_each([], _String) ->
    undefined;
try_each([F|T], String) ->
    case F(String) of
        undefined ->
            try_each(T, String);
        Result ->
            Result
    end.

-spec parse_time(string()) -> parsed_time()|'undefined'.
parse_time(Time) ->
    parse_time(re_list(), Time).

-spec parse_time(list(), string()) -> parsed_time()|'undefined'.
parse_time(REs, Time) ->
    match_time_re(
      re:run(Time, proplists:get_value(time, REs),
             [{capture, ['HOUR', 'MINUTE', 'SECOND',
                         'FRACTIONAL',
                         'TZ', 'TZH', 'TZM'],
               list}])
     ).

-spec parse_date(string()) -> parsed_date()|'undefined'.
parse_date(Date) ->
    parse_date(re_list(), Date).

-spec parse_date(list(), string()) -> parsed_date()|'undefined'.
parse_date(_REs, Date) when length(Date) == 6 ->
    %% Per ISO 8601, YYYYMM *must* include a separator
    undefined;
parse_date(REs, Date) ->
    try_each([fun(D) -> match_calendar(REs, D) end,
              fun(D) -> match_ordinal(REs, D) end,
              fun(D) -> match_week(REs, D) end], Date).

-spec parse_datetime(string()) -> parsed_datetime()|'undefined'.
parse_datetime(DateTime) ->
    parse_datetime(re_list(), DateTime).

-spec parse_datetime(list(), string()) -> parsed_datetime()|'undefined'.
parse_datetime(REs, DateTime) ->
    try_each([fun(D) -> match_calendar_time(REs, D) end,
              fun(D) -> match_ordinal_time(REs, D) end,
              fun(D) -> match_week_time(REs, D) end], DateTime).

-spec parse_tz(string()) -> parsed_timezone()|'undefined'.
parse_tz(TZ) ->
    parse_tz(re_list(), TZ).

-spec parse_tz(list(), string()) -> parsed_timezone()|'undefined'.
parse_tz(REs, TZ) ->
    match_timezone(REs, TZ).

match_week(REs, Date) ->
    match_week_re(
      re:run(Date, proplists:get_value(week_date, REs),
             [{capture, ['YEAR', 'WEEK', 'DAY'], list}])
     ).

match_ordinal(REs, Date) ->
    match_ordinal_re(
      re:run(Date, proplists:get_value(ordinal_date, REs),
             [{capture, ['YEAR', 'DAY'], list}])
     ).

match_calendar(REs, Date) ->
    match_calendar_re(
      re:run(Date, proplists:get_value(calendar_date, REs),
             [{capture, ['YEAR', 'MONTH', 'DAY'], list}])
     ).

match_ordinal_time(REs, DateTime) ->
    match_ordinal_time_re(
      re:run(DateTime, proplists:get_value(ordinal_datetime, REs),
             [{capture, ['YEAR', 'DAY',
                         'HOUR', 'MINUTE', 'SECOND',
                         'FRACTIONAL', 'TZ', 'TZH', 'TZM'],
               list}])
     ).

match_week_time(REs, DateTime) ->
    match_week_time_re(
      re:run(DateTime, proplists:get_value(week_datetime, REs),
             [{capture, ['YEAR', 'WEEK', 'DAY',
                         'HOUR', 'MINUTE', 'SECOND',
                         'FRACTIONAL', 'TZ', 'TZH', 'TZM'],
               list}])
     ).

match_calendar_time(REs, DateTime) ->
    match_calendar_time_re(
      re:run(DateTime, proplists:get_value(calendar_datetime, REs),
             [{capture, ['YEAR', 'MONTH', 'DAY',
                         'HOUR', 'MINUTE', 'SECOND',
                         'FRACTIONAL', 'TZ', 'TZH', 'TZM'],
               list}])
     ).

match_calendar_re(nomatch) ->
    undefined;
match_calendar_re({match, [Year, Month, Day]}) ->
    #parsed_calendar{year=Year,
                     month=maybe_undefined(Month),
                     day=maybe_undefined(Day)}.

match_calendar_time_re(nomatch) ->
    undefined;
match_calendar_time_re({match, [Year, Month, Day,
                                Hour, Minute, Second,
                                Fractional,
                                TZ, TZH, TZM]}) ->
    #parsed_datetime{
       date=match_calendar_re({match, [Year, Month, Day]}),
       time=match_time_re({match, [Hour, Minute, Second,
                                   Fractional, TZ, TZH, TZM]})
      }.

match_week_time_re(nomatch) ->
    undefined;
match_week_time_re({match, [_Year, _Week, _Day,
                            _Hour, _Minute, _Second,
                            _Fractional,
                            _TZ, _TZH, _TZM]}) ->
    week_dates_unsupported.

match_ordinal_time_re(nomatch) ->
    undefined;
match_ordinal_time_re({match, [Year, Day,
                               Hour, Minute, Second,
                               Fractional,
                               TZ, TZH, TZM]}) ->
    #parsed_datetime{
       date=match_ordinal_re({match, [Year, Day]}),
       time=match_time_re({match, [Hour, Minute, Second,
                                   Fractional, TZ, TZH, TZM]})
      }.

match_week_re(nomatch) ->
    undefined;
match_week_re({match, [_Year, _Week, _Day]}) ->
    week_dates_unsupported.

match_ordinal_re(nomatch) ->
    undefined;
match_ordinal_re({match, [Year, Day]}) ->
    #parsed_ordinal{
       year=Year,
       day=Day
      }.

build_parsed_timezone([], _, _) ->
    undefined;
build_parsed_timezone(Label, Hours, Minutes) ->
    #parsed_timezone{
       label=Label,
       hours=maybe_undefined(Hours),
       minutes=maybe_undefined(Minutes)
      }.

match_time_re(nomatch) ->
    undefined;
match_time_re({match, [Hour, Minute, Second,
                       Fraction, TZ, TZH, TZM]}) ->
    #parsed_time{
       hour=Hour,
       minute=maybe_undefined(Minute),
       second=maybe_undefined(Second),
       fraction = case Fraction of
                      [] -> undefined;
                      Value -> #parsed_fraction{value=Value}
                  end,
       timezone = build_parsed_timezone(TZ, TZH, TZM)
      }.

match_timezone(REs, Timezone) ->
    match_timezone_re(
      re:run(Timezone, proplists:get_value(timezone, REs),
             [{capture, ['TZ', 'TZH', 'TZM'], list}])
     ).

match_timezone_re(nomatch) ->
    undefined;
match_timezone_re({match, [TZ, TZH, TZM]}) ->
    build_parsed_timezone(TZ, TZH, TZM).

%% XXX Should also support printing parsed but not compiled tuples so
%% we can output ordinal dates (and eventually week dates).
%%
%% Options:
%%   {format, basic|extended} (terminology from spec, latter means include separators, is default)
%%   {t_prefix, true|false} (false is default. Would like to support `ambiguous' - insert where needed)
%%   {datetime_separator, t|space} (t is default)
%%   {decimal_mark, comma|period} (period is default)
%%   {z, true|false} (true is default, false means use "+00:00" or "+0000")

-spec to_string(compiled_record()|calendar:datetime()) -> string().
to_string(Record) ->
    to_string(Record, [{format, extended}]).

to_string({{_Year, _Month, _Day}=Date, {_Hour, _Minute, _Second}=Time}, Options) ->
    to_string(#datetime{date=jam_erlang:tuple_to_record(#date{}, Date),
                        time=jam_erlang:tuple_to_record(#time{}, Time)},
              Options);
to_string(#time{fraction=Frac,timezone=Tz}=Time, Options) ->
    UseSeparators = proplists:get_value(format, Options, extended) == extended,

    lists:flatten([t_if_opted(lists:keyfind(t_prefix, 1, Options)),
                   render_time(jam_erlang:to_erlangish_time(Time), UseSeparators),
                   render_fraction(Frac, Options),
                   render_tz(Tz, UseSeparators, Options)]);
to_string(#date{}=Date, Options) ->
    UseSeparators = proplists:get_value(format, Options, extended) == extended,

    render_date(jam_erlang:to_erlangish_date(Date), UseSeparators);
to_string(#datetime{date=Date, time=Time}, Options) ->
    DateTimeDivider =
        date_time_divider(lists:keyfind(datetime_separator, 1, Options)),
    to_string(Date, Options) ++ DateTimeDivider ++
        to_string(Time, lists:keydelete(t_prefix, 1, Options));
to_string(#timezone{}=Tz, Options) ->
    UseSeparators = proplists:get_value(format, Options, extended) == extended,
    render_tz(Tz, UseSeparators, Options).

date_time_divider({datetime_separator, t}) ->
    "T";
date_time_divider({datetime_separator, space}) ->
    " ";
date_time_divider(false) ->
    "T".

t_if_opted(false) ->
    "";
t_if_opted({t_prefix, false}) ->
    "";
t_if_opted({t_prefix, true}) ->
    "T".

%% Nearly all date/time components are 2 digits in width, year being
%% the exception.
int_to_str(Int) ->
    int_to_str(Int, 2).

int_to_str(undefined, _Width) ->
    "";
int_to_str(Int, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ ".10.0B",
    lists:flatten(io_lib:format(Format, [Int])).

render_date(Date, UseSeparator) ->
    DateComponents = tuple_to_list(Date),
    string:join([int_to_str(hd(DateComponents), 4)] ++
                    lists:map(fun(Int) -> int_to_str(Int, 2) end,
                              lists:filter(fun(undefined) -> false;
                                              (_) -> true
                                           end, tl(DateComponents))),
                date_separator(UseSeparator)).

render_time(Time, UseSeparator) ->
    string:join(lists:map(fun int_to_str/1,
                          lists:filter(fun(undefined) -> false;
                                          (_) -> true
                                       end, tuple_to_list(Time))),
                time_separator(UseSeparator)).

time_separator(true) ->
    ":";
time_separator(false) ->
    "".

date_separator(true) ->
    "-";
date_separator(false) ->
    "".

decimal_mark({decimal_mark, comma}) ->
    ",";
decimal_mark({decimal_mark, period}) ->
    ".";
decimal_mark(false) ->
    ".".

render_fraction(undefined, _Options) ->
    "";
render_fraction(#fraction{value=0.0}, _Options) ->
    "";
render_fraction(#fraction{value=Fraction, precision=Precision}, Options) ->
    AsInt = drop_zeroes(round(Fraction * math:pow(10, Precision))),
    AsList = integer_to_list(AsInt),
    %% Use string:right/3 in the eventuality that the fraction when
    %% represented as an integer will lose 0s to the left, e.g. ".023"
    %% converts to the integer 23.
    decimal_mark(lists:keyfind(decimal_mark, 1, Options)) ++
        string:right(AsList, Precision, $0).

drop_zeroes(0) ->
    0;
drop_zeroes(Int) ->
    drop_zeroes(Int, Int div 10, Int rem 10).

drop_zeroes(_Prev, Next, 0) ->
    drop_zeroes(Next, Next div 10, Next rem 10);
drop_zeroes(Prev, _Next, _Rem) ->
    Prev.

%% Could allow `z' to be an integer (2 or 4) to indicate whether we
%% want "+00" or "+0000" (or "+00:00" with separators) but that seems
%% overkill.
pick_z({z, true}, _) ->
    "Z";
pick_z({z, false}, true) ->
    "+00:00";
pick_z({z, false}, false) ->
    "+0000";
pick_z(false, _) ->
    "Z".

tz_sign(X) when X =< 0 ->
    "+";
tz_sign(_) ->
    "-".

render_tz(undefined, _UseSeparators, _Options) ->
    "";
render_tz(#timezone{label="Z"}, UseSeparators, Options) ->
    pick_z(lists:keyfind(z, 1, Options), UseSeparators);
render_tz(#timezone{hours=0, minutes=0}, UseSeparators, Options) ->
    pick_z(lists:keyfind(z, 1, Options), UseSeparators);
render_tz(#timezone{hours=Hours, minutes=Minutes}, UseSeparators, _Options) ->
    %% Most efficiently we'd just use the second element of the tuple,
    %% the original timezone specification, but we need to take
    %% separators into account
    tz_sign(Hours) ++
        string:join(lists:map(fun int_to_str/1,
                              lists:filter(fun(undefined) -> false;
                                              (_) -> true
                                           end, [abs(Hours), abs(Minutes)])),
                    time_separator(UseSeparators)).

%% Failed regular expression groups arrive as empty lists
maybe_undefined([]) ->
    undefined;
maybe_undefined(Value) ->
    Value.

-ifdef(TEST).
tz_labels_test_() ->
    Mappings = [
                {"+00", "Z"},
                {"+15", "+15:00"},
                {"-1230", "-12:30"}
               ],
    lists:map(fun({Input, Output}) ->
                          ?_assertEqual(Output, to_string(jam:compile(parse_tz(Input))))
                  end, Mappings).

erlang_tuples_string_test_() ->
    Mappings = [
                {{{1825, 5, 11}, {0, 13, 57}}, "1825-05-11T00:13:57"},
                {{{93, 12, 1}, {23, 10, 5}}, "0093-12-01T23:10:05"}
               ],
    lists:map(fun({Erlang, String}) ->
                          ?_assertEqual(String, to_string(Erlang))
                  end, Mappings).

not_really_dates_test_() ->
    UnparseableDates = ["201411", "2014Z", "20", "201", "T2014",
                        "2014-W25-01", "2015-1212"],
    lists:map(fun(D) -> ?_assertEqual(undefined, parse_date(D)) end,
              UnparseableDates).

not_really_times_test_() ->
    UnparseableTimes = ["10:10:", "10:10:10:10", "10:10 +05",
                        "10:10:10 +05", "TT10:10"],

    lists:map(fun(T) -> ?_assertEqual(undefined, parse_time(T)) end,
              UnparseableTimes).

dates_in_june_test_() ->
    Dates = ["2121-06-30", "2016-06", "2011-06", "20110617",
             "2016-180", "1980155"],
    DateTimes = ["2121-06-30T0529", "2016-06-29T15", "20110617T22:20:05-11",
                 "2016-181T00:00:00+01"],

    lists:map(fun(D) -> ?_assertMatch({_, 6, _},
                                      jam_erlang:record_to_tuple(jam:compile(parse(D)))) end,
              Dates)
        ++
    lists:map(fun(DT) -> DateTime = jam:compile(parse(DT)),
                         ?_assertMatch({_, 6, _},
                                       jam_erlang:record_to_tuple(DateTime#datetime.date)) end,
              DateTimes).

valid_test_() ->
    ValidTimes = ["24:00", "24:00:00", "T24.0", "23:59:60", "T00", "0100"],
    BuggyTimes = ["24:00:01", "23:59:61", "25"],
    ValidDates = ["2000-01-30", "2012-02-29", "0001-12-03", "1492-05", "1492-012", "1492-366", "1493-365"],
    BuggyDates = ["2015-00", "2015-01-00", "1900-366", "1900-02-29", "2016-13-01", "1492-13", "1492-367", "1493-366"],
    lists:map(fun(T) -> ?_assert(jam:is_valid(jam:compile(parse(T)))) end,
              ValidTimes)
        ++
    lists:map(fun(T) -> ?_assert(not jam:is_valid(jam:compile(parse(T)))) end,
              BuggyTimes)
        ++
    lists:map(fun(D) -> ?_assert(jam:is_valid(jam:compile(parse(D)))) end,
              ValidDates)
        ++
    lists:map(fun(D) -> ?_assert(not jam:is_valid(jam:compile(parse(D)))) end,
              BuggyDates).

-endif.
