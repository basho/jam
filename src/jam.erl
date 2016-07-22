%% -------------------------------------------------------------------
%%
%% jam: Date/time processing.
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

-module(jam).

-include("jam_internal.hrl").

-export([
         compile/1, compile/2,
         round_fractional_seconds/1, offset_round_fractional_seconds/1,
         expand/2,
         convert_tz/2, offset_convert_tz/2,
         is_valid/1, is_valid/2, is_complete/1,
         normalize/1, offset_normalize/1,
         to_epoch/1, to_epoch/2,
         from_epoch/1, from_epoch/2,
         tz_to_seconds/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%% Compiling

%% The compiling step converts the strings captured by parsing into a
%% (possibly valid) date and/or time. The resulting tuples will not be
%% the same as Erlang's date/time tuples because this library permits
%% fractional seconds and returns time zones as an explicit value for
%% further processing.

%% Per ISO 8601, a time such as "14:10" does *not* imply
%% "14:10:00". Instead, the first string is described as "reduced
%% accuracy". Similarly, "2016-06" and "2016-W15" are examples of
%% reduced accuracy dates.
%%
%% The compiling functions take an optional accuracy parameter,
%% `minimum_accuracy'.
%%
%% If the result is not sufficiently accurate to meet any minimum
%% accuracy requirement supplied, the atom `incomplete_time' or
%% `incomplete_date' will be returned instead of a new structure.
%%
%% The processing functions also take a default time zone as a
%% parameter, expressed as ISO 8601-compliant timezone strings ("Z",
%% "+04:30", "-05", etc). This will be ignored if a time zone is
%% already part of the parsed data.

%%% Validation

%% The compiling step only exercises as much knowledge about "real"
%% times and dates as is necessary (e.g., knowing what years are leap
%% years to properly interpret ordinal dates). A time such as "25:15"
%% is a possible outcome of the parsing and compiling steps, so
%% validation functions are supplied which will return `true' or
%% `false' if the date/time is legitimate.

%% If the `leap_second_midnight' parameter is supplied to the
%% validation function, a second of `60' will only be allowed if the
%% hour is 23 and minute is 59, but generally speaking we will not
%% always know what time zone the time is being expressed as, so that
%% requirement will not be enforced by default (and thus 60 seconds
%% will be considered valid).
%%
%% No more than one leap second has ever been added to the same day,
%% so this library will treat any number of seconds greater than 60 as
%% invalid.

%%% Normalization

%% There are two unusual times which may arise.
%%
%% The first, permitted by ISO 8601, is "24:00". This is *not* the
%% same as "00:00", at least when also attached to a date. "2016-06-15
%% 24:00" is the same as "2016-06-16 00:00" and normalization will
%% convert the former into the latter.
%%
%% The second unusual time value: seconds > 59.
%%
%% Occasionally leap seconds
%% (https://en.wikipedia.org/wiki/Leap_second) will be added at the
%% end of a day. UNIX/POSIX time will silently "absorb" that into the
%% first second of the following day
%% (https://en.wikipedia.org/wiki/Unix_time#Leap_seconds).
%%
%% So, seconds == 60 are allowed in the validation step and converted
%% to 00 seconds in the following minute in the normalization step.

%%% Conversion to epoch seconds

%% Functions will be provided to convert the time/date structures to
%% UNIX epoch time at a customizable level of granularity. Since
%% fractional times are supported, it will be possible to express
%% millisecond (or larger, or smaller) values in time strings.

%% list_to_integer (but leave `undefined' intact)
l2i(undefined) ->
    undefined;
l2i(Value) ->
    list_to_integer(Value).

%% undefined_to_zero
u2z(undefined) ->
    0;
u2z(Int) ->
    Int.

-define(YEAR_ACCURACY,   20).
-define(MONTH_ACCURACY,  18).
-define(DAY_ACCURACY,    16).
-define(HOUR_ACCURACY,   14).
-define(MINUTE_ACCURACY, 12).
-define(SECOND_ACCURACY, 10).

accuracy(undefined) ->
    undefined;
accuracy(year) ->
    ?YEAR_ACCURACY;
accuracy(month) ->
    ?MONTH_ACCURACY;
accuracy(day) ->
    ?DAY_ACCURACY;
accuracy(hour) ->
    ?HOUR_ACCURACY;
accuracy(minute) ->
    ?MINUTE_ACCURACY;
accuracy(second) ->
    ?SECOND_ACCURACY.

make_date_record({Year, Month, Day}) ->
    #date{year=Year, month=Month, day=Day}.

preprocess(#parsed_ordinal{year=Year, day=Day}, _Options) ->
    %% Unlike other date and time formats, ordinal dates are
    %% necessarily complete (to the day, anyway) so we'll convert to a
    %% calendar date before checking accuracy
    make_date_record(calculate_ordinal_date({Year, Day}));
preprocess(#parsed_time{timezone=undefined}=Time, Options) ->
    %% Replace an undefined timezone with a default if passed as an option
    Time#parsed_time{timezone=maybe_default_timezone(proplists:get_value(default_timezone, Options))};
preprocess(Tuple, _Options) ->
    Tuple.

maybe_default_timezone(undefined) ->
    undefined;
maybe_default_timezone(Timezone) ->
    jam_iso8601:parse_tz(Timezone).

-spec compile(parsed_time()) -> time_record();
             (parsed_datetime()) -> datetime_record();
             (parsed_date()) -> date_record();
             (parsed_timezone()) -> timezone();
             ('undefined') -> 'undefined'.
%% @equiv compile(Record, [])
compile(Record) ->
    compile(Record, []).

%% @doc Convert the string-based output from a parser into numeric
%% values.
%%
%% Parameters that can be supplied in the optional `Options' list:
%%
%% <ul>
%%  <li>`{minimum_accuracy, Accuracy}'</li>
%%  <li>`{default_timezone, TZstring}'</li>
%% </ul>
%%
%% Accuracy values are atoms ranging from `year' to `second' that
%% indicate the granularity that must be captured in the output
%% of the parser.
%%
%% For example, specifying `minute' as a minimum accuracy means that a
%% legitimate ISO 8601 string like "2010-05-03T06" could not be
%% compiled and would result in an `incomplete_date' error.
%%
%% The default timezone string must be ISO 8601-compliant.
-spec compile(parsed_time(), list()) -> time_record();
             (parsed_datetime(), list()) -> datetime_record();
             (parsed_date(), list()) -> date_record()|datetime_record();
             (parsed_timezone(), list()) -> timezone();
             ('undefined', list()) -> 'undefined'.
compile(undefined, _Options) ->
    undefined;
compile(#parsed_timezone{}=TZ, _Options) ->
    compile_timezone(TZ);
compile(#parsed_datetime{date=Date, time=Time}, Options) ->
    finish_compile(check_accuracy(preprocess(Date, Options), preprocess(Time, Options), accuracy(proplists:get_value(minimum_accuracy, Options))));
compile(#parsed_calendar{}=Date, Options) ->
    finish_compile(check_accuracy(preprocess(Date, Options), undefined, accuracy(proplists:get_value(minimum_accuracy, Options))));
compile(#parsed_ordinal{}=Date, Options) ->
    finish_compile(check_accuracy(preprocess(Date, Options), undefined, accuracy(proplists:get_value(minimum_accuracy, Options))));
compile(#parsed_time{}=Time, Options) ->
    finish_compile(check_accuracy(undefined, preprocess(Time, Options), accuracy(proplists:get_value(minimum_accuracy, Options)))).

%% `check_accuracy/4' will make certain any minimum accuracy is met
check_accuracy(Date, Time, undefined) ->
    %% 3rd parameter is minimum accuracy. If there is no minimum
    %% specified, we don't need to check anything here
    {Date, Time};
check_accuracy(_Date, undefined, Minimum) when Minimum < ?DAY_ACCURACY ->
    %% If we don't have a time, and our minimum accuracy is time-related, bail
    incomplete_time;
check_accuracy(#parsed_calendar{month=undefined}, _Time, Minimum)
  when Minimum < ?YEAR_ACCURACY ->
    incomplete_date;
check_accuracy(#parsed_calendar{day=undefined}, _Time, Minimum)
  when Minimum < ?MONTH_ACCURACY ->
    %% Perhaps we should respond `incomplete_time' if the minimum
    %% accuracy is hour/minute/second and we have an incomplete date
    %% but I prefer this
    incomplete_date;
check_accuracy(_Date, #parsed_time{minute=undefined, fraction=undefined}, Minimum)
  when Minimum < ?HOUR_ACCURACY ->
    %% If we have a fractional time, we'll consider it good enough to
    %% make minute/second accuracy, so this clause only triggers on
    %% undefined fractions
    incomplete_time;
check_accuracy(_Date, #parsed_time{second=undefined, fraction=undefined}, Minimum)
  when Minimum < ?MINUTE_ACCURACY ->
    %% If we have a defined fraction, we'll consider it good enough to
    %% make minute/second accuracy, so this clause only triggers on
    %% undefined fractions
    incomplete_time;
check_accuracy(Date, Time, _Minimum) ->
    {Date, Time}.

%% All errors are atoms; rather than create a partial datetime record
%% with an error atom nested inside, make certain we return the error
%% directly.
expand_to_datetime(Date, _Time) when is_atom(Date) ->
    Date;
expand_to_datetime(_Date, Time) when is_atom(Time) ->
    Time;
expand_to_datetime(Date, Time) ->
    #datetime{date=Date, time=Time}.

-spec expand(compiled_record(), accuracy()) -> compiled_record();
            ('undefined', accuracy()) -> 'undefined'.
expand(undefined, _Target) ->
    undefined;
expand(Record, Target) when is_atom(Target) ->
    expand_2(Record, accuracy(Target)).

%% Names are hard. Step 2 of the expansion process forces a datetime
%% structure when necessary, and regardless continues to step 3.
expand_2(#date{}=Date, Target) when Target < ?DAY_ACCURACY ->
    %% Must create a time record for time-based expansion requirements
    expand_to_datetime(expand_3(Date, Target), expand_3(#time{}, Target));
expand_2(#datetime{date=Date, time=undefined}, Target) when Target < ?DAY_ACCURACY ->
    %% Defer to the previous function clause to create a new time record
    expand_2(Date, Target);
expand_2(#datetime{date=Date, time=Time}, Target) ->
    expand_to_datetime(expand_3(Date, Target), expand_3(Time, Target));
expand_2(Record, Target) ->
    expand_3(Record, Target).

%% Step 3 of the expansion process: populate lower bound values to
%% meet the desired accuracy. We only need concern ourselves with date
%% or time records; step 2 will make handle any datetime records.
expand_3(#date{year=undefined}, _Target) ->
    incomplete_date;
expand_3(#date{}=Date, ?YEAR_ACCURACY) ->
    Date;
expand_3(#date{month=undefined}=Date, Target) when Target < ?YEAR_ACCURACY->
    expand_3(Date#date{month=1}, Target);
expand_3(#date{day=undefined}=Date, Target) when Target < ?MONTH_ACCURACY ->
    Date#date{day=1};

%% We will consider a fractional value to satisfy any expansion target
expand_3(#time{fraction=Fraction}=Time, _Target) when Fraction /= undefined ->
    Time;

expand_3(#time{hour=undefined}=Time, Target) when Target =< ?HOUR_ACCURACY ->
    expand_3(Time#time{hour=0}, Target);
expand_3(#time{minute=undefined}=Time, Target) when Target =< ?MINUTE_ACCURACY ->
    expand_3(Time#time{minute=0}, Target);
expand_3(#time{second=undefined}=Time, Target) when Target =< ?SECOND_ACCURACY ->
    expand_3(Time#time{second=0}, Target);

expand_3(Record, _Target) ->
    Record.

-spec round_fractional_seconds(compiled_record()) -> compiled_record();
                              ('undefined') -> 'undefined'.
round_fractional_seconds(Record) ->
    {_Adjust, NewRecord} = offset_round_fractional_seconds(Record),
    NewRecord.

%% The integer returned as the first element of the tuple indicates
%% whether the time rolled over to midnight: 1 for a 1 day increase, 0
%% otherwise.
%%
%% If a datetime tuple is provided, the date element will be
%% incremented in the return value if applicable.
-spec offset_round_fractional_seconds(compiled_record()) ->
                                             {0|1, compiled_record()};
                                     ('undefined') -> {0, 'undefined'}.
offset_round_fractional_seconds(undefined) ->
    {0, undefined};
offset_round_fractional_seconds(#time{fraction=undefined}=Time) ->
    {0, Time};
offset_round_fractional_seconds(#time{fraction=#fraction{value=Frac}}=Time) when Frac >= 0.5 ->
    {DateBump, NewTime} =
        jam_math:add_time(jam_erlang:to_time(Time), {0, 0, 1}),
    {DateBump, jam_erlang:tuple_to_record(Time#time{fraction=undefined}, NewTime)};
offset_round_fractional_seconds(#datetime{date=Date, time=Time}) ->
    {DateAdj, NewTime} = offset_round_fractional_seconds(Time),
    NewDate = jam_math:add_date(jam_erlang:to_date(Date), DateAdj),
    {DateAdj, #datetime{date=jam_erlang:tuple_to_record(#date{}, NewDate), time=NewTime#time{fraction=undefined}}};
offset_round_fractional_seconds(DateTime) ->
    {0, DateTime}.

-spec convert_tz(compiled_record(), string()) -> compiled_record();
                ('undefined', string()) -> 'undefined'.
convert_tz(Record, TZ) ->
    {_Adjust, NewRecord} = offset_convert_tz(Record, TZ),
    NewRecord.

-spec offset_convert_tz(compiled_record(), string()) -> {-1|0|1, compiled_record()};
                ('undefined', string()) -> {0, 'undefined'}.
%% Like other `offset_` functions this will return a tuple with an
%% initial value that indicates whether the date changed as a result
%% of changing time zones, and if supplied, the date will be
%% transformed.
%%
%% The new timezone argument must be a valid ISO 8601 timezone, so: +
%% or - is required, and hours/minutes must be 2 digits with an
%% optional : separator.
offset_convert_tz(undefined, _NewTz) ->
    {0, undefined};
offset_convert_tz(#datetime{time=#time{timezone=undefined}}, _NewTz) ->
    {0, undefined};
offset_convert_tz(#datetime{date=Date, time=Time}, NewTz) ->
    {DateAdj, NewTime} = offset_convert_tz(Time, NewTz),
    NewDate = jam_math:add_date(jam_erlang:to_date(Date), DateAdj),
    {DateAdj, #datetime{date=jam_erlang:tuple_to_record(#date{}, NewDate), time=NewTime}};
offset_convert_tz(#time{timezone=undefined}, _NewTz) ->
    {0, undefined};
offset_convert_tz(#time{}=Time, NewTz) ->
    convert_compiled_tz(Time, compile(jam_iso8601:parse_tz(NewTz))).

convert_compiled_tz(#time{timezone=TzRec}=Time, TzRec) ->
    {0, Time};
convert_compiled_tz(#time{timezone=#timezone{hours=AddH, minutes=AddM}}=Time,
                     #timezone{label="Z"}=NewTz) ->
    %% The old timezone has integer values expressed as values to add
    %% to reach UTC, so this case is simple
    {DateAdj, NewTime} =
        jam_math:add_time(jam_erlang:to_time(Time), {AddH, AddM}),
    {DateAdj, jam_erlang:tuple_to_record(Time#time{timezone=NewTz}, NewTime)};
convert_compiled_tz(#time{timezone=#timezone{hours=OldAddH, minutes=OldAddM}}=Time,
                     #timezone{hours=NewAddH, minutes=NewAddM}=NewTz) ->
    %% We convert to UTC first, then to the new timezone by inverting
    %% the sign on the values to add.
    {UTCAdj, UTCTime} =
        jam_math:add_time(jam_erlang:to_time(Time), {OldAddH, OldAddM}),
    {NewAdj, NewTime} =
        jam_math:add_time(UTCTime, {-NewAddH, -NewAddM}),
    {NewAdj + UTCAdj, jam_erlang:tuple_to_record(Time#time{timezone=NewTz}, NewTime)}.



finish_compile(Error) when is_atom(Error) ->
    Error;
finish_compile({undefined, Time}) ->
    compile_time(Time);
finish_compile({Date, undefined}) ->
    compile_date(Date);
finish_compile({Date, Time}) ->
    #datetime{date=compile_date(Date),
              time=compile_time(Time)}.

compile_date(#parsed_calendar{year=Year, month=Month, day=Day}) ->
    jam_erlang:tuple_to_record(#date{}, {l2i(Year), l2i(Month), l2i(Day)});
compile_date(Date) ->
    Date.

compile_time(undefined) ->
    undefined;
compile_time(#parsed_time{fraction=undefined, timezone=TZ}=Time) ->
    {Hour, Minute, Second} = jam_erlang:to_time(Time),
    jam_erlang:tuple_to_record(#time{timezone=compile_timezone(TZ)},
                               {l2i(Hour), l2i(Minute), l2i(Second)});
compile_time(#parsed_time{fraction=#parsed_fraction{value=Fractional},
                          timezone=TZ}=Time) ->
    {Hour, Minute, Second} = jam_erlang:to_time(Time),
    %% Figure out what to do with the fractional value. Whatever is
    %% left once we start applying it has to get passed down the chain.
    Frac = {list_to_float("0." ++ Fractional), length(Fractional)},
    {Min, FracRemainder} = maybe_fractional(Minute, Frac),
    {Sec, FracRemainder2} = maybe_fractional(Second, FracRemainder),
    jam_erlang:tuple_to_record(
      #time{fraction=jam_erlang:tuple_to_record(#fraction{}, FracRemainder2),
            timezone=compile_timezone(TZ)},
      {l2i(Hour), Min, Sec}).

utc_timezone_record() ->
    #timezone{label="Z", hours=0, minutes=0}.

%% We want the integer `timezone' fields to represent adjustments
%% necessary to convert to UTC, so India, with a +05:30 time zone,
%% will map to `{timezone, "+05:30", -5, -30}' or `{timezone, "+0530", -5, -30}'.
compile_timezone(undefined) ->
    undefined;
compile_timezone(#parsed_timezone{label="Z"}) ->
    utc_timezone_record();
compile_timezone(#parsed_timezone{label=TZ, hours=TZH, minutes=TZM}) ->
    HourOffset = -l2i(TZH),
    #timezone{label=TZ, hours=HourOffset,
              minutes=timezone_minute_offset(HourOffset, TZM)}.

%% Must match sign of minutes to sign of hour
timezone_minute_offset(Hour, Minute) when Hour < 0 ->
    -u2z(l2i(Minute));
timezone_minute_offset(_Hour, Minute) ->
    u2z(l2i(Minute)).

%% Apply the fractional component once we figure out what the original
%% time string left unspecified
maybe_fractional(undefined, {Frac, Precision}) ->
    FloatUnits = 60.0 * Frac,
    IntUnits = trunc(FloatUnits),
    Remainder = {FloatUnits - IntUnits, Precision},
    {IntUnits, Remainder};
maybe_fractional(Value, Frac) ->
    {list_to_integer(Value), Frac}.

-spec is_complete(compiled_record()|parsed_record()) -> boolean().
is_complete(#datetime{date=Date, time=Time}) ->
    is_complete(Date) andalso is_complete(Time);
is_complete(#parsed_datetime{date=Date, time=Time}) ->
    is_complete(Date) andalso is_complete(Time);
is_complete(#time{second=undefined}) ->
    false;
is_complete(#time{}) ->
    true;
is_complete(#parsed_time{second=undefined}) ->
    false;
is_complete(#parsed_time{}) ->
    true;
is_complete(#date{day=undefined}) ->
    false;
is_complete(#date{}) ->
    true;
is_complete(#parsed_ordinal{}) ->
    true;
is_complete(#parsed_calendar{day=undefined}) ->
    false;
is_complete(#parsed_calendar{}) ->
    true.

-spec is_valid(compiled_record()|timezone()|'undefined') -> boolean().
is_valid(Record) ->
    is_valid(Record, []).

-spec is_valid(compiled_record()|timezone()|'undefined', list()) -> boolean().
is_valid(undefined, _Options) ->
    false;
is_valid(#datetime{date=Date, time=Time}, Options) ->
    is_valid_date(Date, Options) andalso is_valid_time(Time, Options);
is_valid(#date{}=Date, Options) ->
    is_valid_date(Date, Options);
is_valid(#time{}=Time, Options) ->
    is_valid_time(Time, Options);
is_valid(#timezone{}=TZ, Options) ->
    is_valid_timezone(TZ, Options).

is_valid_date(#date{year=Year, month=undefined, day=undefined}, _Options)
  when is_integer(Year) ->
    true;
is_valid_date(#date{month=Month, day=undefined}, _Options) ->
    Month > 0 andalso Month < 13;
is_valid_date(#date{}=Date, _Options) ->
    calendar:valid_date(jam_erlang:to_date(Date)).

%% A minute with 61 seconds (thus `second=60') can happen when leap
%% seconds are added. Leap seconds are added at midnight UTC.
%%
%% We don't always know what timezone we're evaluating, thus it is
%% configurable whether or not the `is_valid_time/2' function will
%% enforce the midnight-only constraint (second parameter).
midnight_leap_second({23, 59, 60}, true) ->
    true;
midnight_leap_second({_, _, 60}, false) ->
    true;
midnight_leap_second(_, _) ->
    false.

is_valid_time_tuple({Hour, Minute, Second}, LeapSecondMustBeMidnight) ->
    Hour > -1
        andalso (Hour < 24 orelse (Hour == 24 andalso Minute + Second == 0))
        andalso Minute > -1 andalso Minute < 60
        andalso Second > -1
        andalso (Second < 59 orelse
                 midnight_leap_second({Hour, Minute, Second},
                                      LeapSecondMustBeMidnight)).

is_valid_time(#time{hour=Hour, minute=undefined, second=undefined}, _Options) ->
    is_valid_time_tuple({Hour, 0, 0}, false);
is_valid_time(#time{hour=Hour, minute=Minute, second=undefined}, _Options) ->
    is_valid_time_tuple({Hour, Minute, 0}, false);
is_valid_time(#time{}=Time, Options) ->
    is_valid_time_tuple(jam_erlang:to_time(Time),
                        lists:member(leap_second_midnight, Options)).

%% As of this writing, the valid time zone range is from -1200 to
%% +1400. Since politicians love to mess with this, going to treat
%% 1500 as an absolute maximum and hope for the best.
is_valid_timezone(#timezone{}=TZ, _Options) ->
    abs(tz_to_seconds(TZ)) =< 15 * 3600.

-spec normalize('undefined') -> 'undefined';
               (date_record()) -> date_record();
               (time_record()) -> time_record();
               (datetime_record()) -> datetime_record().
normalize(Record) ->
    {_Adjust, NewRecord} = offset_normalize(Record),
    NewRecord.

-spec offset_normalize('undefined') -> {0, 'undefined'};
                      (date_record()) -> {integer(), date_record()};
                      (time_record()) -> {integer(), time_record()};
                      (datetime_record()) -> {integer(), datetime_record()}.
offset_normalize(undefined) ->
    {0, undefined};
offset_normalize(#datetime{date=Date, time=Time}) ->
    {DateAdjust, NewTime} = normalize_time(Time),
    {DateAdjust, #datetime{date=normalize_date(Date, DateAdjust), time=NewTime}};
offset_normalize(#date{}=Date) ->
    {0, Date};
offset_normalize(#time{}=Time) ->
    normalize_time(Time).

normalize_date(#date{}=Date, 0) ->
    Date;
normalize_date(#date{}=Date, Adjust) ->
    jam_erlang:tuple_to_record(
      #date{}, jam_math:add_date(jam_erlang:to_date(Date), Adjust)).


%% Allow for 24:00:00 per the ISO 8601 standard and the occasional
%% leap second.
normalize_time(#time{hour=24}=Time) ->
    {1, Time#time{hour=0}};
normalize_time(#time{hour=Hour, minute=Minute, second=60}=Time) ->
    {DateAdj, NewTime} =
        jam_math:add_time({Hour, Minute, 59}, {0, 0, 1}),
    {DateAdj, jam_erlang:tuple_to_record(Time, NewTime)};
normalize_time(Time) ->
    {0, Time}.

%% Ordinal date calculations stolen from Wikipedia.
%% To the day of   Jan	Feb     Mar     Apr     May     Jun     Jul     Aug     Sep     Oct     Nov     Dec
%% Add             0	31	59	90	120	151	181	212	243	273	304	334
%% Leap years      0	31	60	91	121	152	182	213	244	274	305	335

calculate_ordinal_date({Year, Day}) when is_list(Year) ->
    calculate_ordinal_date({list_to_integer(Year), list_to_integer(Day)});
calculate_ordinal_date({Year, Day}) ->
    calculate_ordinal_date(Year, Day, calendar:is_leap_year(Year)).

calculate_ordinal_date(Year, Day, _Leap) when Day < 32 ->
    {Year, 1, Day};
calculate_ordinal_date(Year, Day, true) when Day < 61 ->
    {Year, 2, Day - 31};
calculate_ordinal_date(Year, Day, false) when Day < 60 ->
    {Year, 2, Day - 31};
calculate_ordinal_date(Year, Day, true) ->
    calculate_ordinal_date_post_feb(Year, Day-1);
calculate_ordinal_date(Year, Day, false) ->
    calculate_ordinal_date_post_feb(Year, Day).

calculate_ordinal_date_post_feb(Year, Day) when Day < 91 ->
    {Year, 3, Day - 59};
calculate_ordinal_date_post_feb(Year, Day) when Day < 121 ->
    {Year, 4, Day - 90};
calculate_ordinal_date_post_feb(Year, Day) when Day < 152 ->
    {Year, 5, Day - 120};
calculate_ordinal_date_post_feb(Year, Day) when Day < 182 ->
    {Year, 6, Day - 151};
calculate_ordinal_date_post_feb(Year, Day) when Day < 213 ->
    {Year, 7, Day - 181};
calculate_ordinal_date_post_feb(Year, Day) when Day < 244 ->
    {Year, 8, Day - 212};
calculate_ordinal_date_post_feb(Year, Day) when Day < 274 ->
    {Year, 9, Day - 243};
calculate_ordinal_date_post_feb(Year, Day) when Day < 305 ->
    {Year, 10, Day - 273};
calculate_ordinal_date_post_feb(Year, Day) when Day < 335 ->
    {Year, 11, Day - 304};
calculate_ordinal_date_post_feb(Year, Day) ->
    {Year, 12, Day - 334}.

-spec to_epoch(date_record()) -> missing_time;
              (time_record()) -> missing_date;
              (datetime_record()) -> integer().
to_epoch(Record) ->
    to_epoch(Record, 0).

%% 2nd argument is the power of 10 reflecting subsecond accuracy. For
%% example, if nanosecond values are required, the epoch value would
%% have to be multiplied by 10^9, so the argument would be 9.
-spec to_epoch(date_record(), integer()) -> missing_time;
              (time_record(), integer()) -> missing_date;
              (datetime_record(), integer()) -> integer().
to_epoch(#date{}, _Precision) ->
    missing_time;
to_epoch(#time{}, _Precision) ->
    missing_date;
to_epoch(#datetime{}=DateTime, Precision) ->
    check_complete_before_conversion(
      is_complete(DateTime),
      DateTime,
      trunc(precision_to_mult(Precision))
     ).

split_epoch(Integer, Precision) ->
    Divisor = trunc(precision_to_mult(Precision)),
    {Integer div Divisor, Integer rem Divisor}.

-spec from_epoch(non_neg_integer()) -> datetime_record().
from_epoch(Epoch) ->
    from_epoch(Epoch, 0).

-spec from_epoch(non_neg_integer(), non_neg_integer()) -> datetime_record().
from_epoch(Epoch, Precision) ->
    {EpochSeconds, Remainder} = split_epoch(Epoch, Precision),
    Fraction = #fraction{value=Remainder / precision_to_mult(Precision), precision=Precision},
    TZ = utc_timezone_record(),
    {Date, Time} = utc_seconds_to_universal_datetime(EpochSeconds),
    #datetime{
       date=jam_erlang:tuple_to_record(#date{}, Date),
       time=jam_erlang:tuple_to_record(#time{fraction=Fraction,
                                             timezone=TZ}, Time)
      }.

utc_seconds_to_universal_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + ?GREGORIAN_MAGIC).

precision_to_mult(Precision) ->
    math:pow(10, Precision).

check_complete_before_conversion(false, _DateTime, _Precision) ->
    incomplete_datetime;
check_complete_before_conversion(true, DateTime, Precision) ->
    ErlangDateTime = convert_to_erlang(DateTime),
    (calendar:datetime_to_gregorian_seconds(ErlangDateTime) - ?GREGORIAN_MAGIC)
        * Precision + extra_precision(DateTime, Precision).

extra_precision(#datetime{time=#time{fraction=undefined}}, _Precision) ->
    0;
extra_precision(#datetime{time=#time{fraction=#fraction{value=Float}}}, Precision) ->
    round(Float * Precision).


%% To do this properly requires both a date and a time with timezone,
%% because the gregorian/epoch conversion requires the time be
%% specified in UTC. XXX: Stricter enforcement?
convert_to_erlang(#datetime{date=Date, time=Time}) ->
    {DateAdjust, NewTime} = timezone_to_utc(Time),
    NewDate = normalize_date(Date, DateAdjust),
    {convert_to_erlang(NewDate), convert_to_erlang(NewTime)};
convert_to_erlang(#date{}=Date) ->
    jam_erlang:to_date(Date);
convert_to_erlang(#time{}=Time) ->
    jam_erlang:to_time(Time).

timezone_to_utc(#time{timezone=undefined}=Time) ->
    {0, Time};
timezone_to_utc(#time{hour=Hour, minute=Minute,
                      timezone=#timezone{hours=HourAdj, minutes=MinuteAdj}}=Time) ->
    {Wrap, NewHour, NewMinute} = adjust_time({Hour, HourAdj}, {Minute, MinuteAdj}),
    {Wrap, Time#time{timezone=utc_timezone_record(), hour=NewHour, minute=NewMinute}}.

adjust_time({Hour, HourAdj}, {Minute, MinuteAdj}) ->
    {ExtraHourAdj, NewMinute} = wrap(Minute+u2z(MinuteAdj), {0, 59}),
    {DayAdj, NewHour} = wrap(Hour+u2z(HourAdj)+ExtraHourAdj, {0, 23}),
    {DayAdj, NewHour, NewMinute}.

wrap(Int, {Min, Max}) when Int < Min ->
    Adj = Min - Int,
    {-1, (Max+1)-Adj};
wrap(Int, {Min, Max}) when Int > Max ->
    Adj = Int - Max,
    {1, Min+(Adj-1)};
wrap(Int, {_Min, _Max}) ->
    {0, Int}.

%% Convert to seconds. Must be the negation of the resulting integer
%% because the timezone record tracks the adjustment necessary to
%% convert to UTC, while users/developers will expect the same sign as
%% the original string
-spec tz_to_seconds(timezone()) -> integer().
tz_to_seconds(#timezone{hours=Hours, minutes=Minutes}) ->
    -(Hours*3600 + Minutes*60).

-ifdef(TEST).
normalize_with_adjust_test_() ->
    EquivWithAdjust = [
                       {#time{hour=0,minute=0,second=0},
                        #time{hour=24,minute=0,second=0}},
                       {#time{hour=0,minute=0,second=0},
                        #time{hour=23,minute=59,second=60}}
                      ],
    lists:map(fun({Normalized, Time}) ->
                      ?_assertEqual({1, Normalized}, offset_normalize(Time))
              end, EquivWithAdjust).

normalize_without_adjust_test_() ->
    NoAdjust = [
                %% Would not expect a datetime to be populated for
                %% just a year, but let's make sure things don't blow
                %% up
                #datetime{date=#date{year=2016},
                          time=#time{}},
                #datetime{date=#date{year=2016},
                          time=undefined},

                #date{year=2016, month=2},
                #time{hour=15, minute=7},
                #time{hour=15}
               ],
    lists:map(fun(Record) ->
                      ?_assertEqual({0, Record}, offset_normalize(Record))
              end, NoAdjust).

tz_valid_test_() ->
    TZs = [
           {#timezone{hours=15, minutes=00}, true},
           {#timezone{hours=15, minutes=01}, false},
           {#timezone{hours=-15, minutes=-00}, true},
           {#timezone{hours=-15, minutes=-01}, false}
          ],
    lists:map(fun({TZ, IsValid}) ->
                      ?_assertEqual(IsValid, is_valid(TZ))
              end, TZs).

tz_offset_test_() ->
    TZs = [
           {#timezone{hours=4, minutes=30}, -16200},
           {#timezone{hours=-12, minutes=-45}, 45900},
           {#timezone{hours=0, minutes=0}, 0}
          ],
    lists:map(fun({TZ, Offset}) ->
                      ?_assertEqual(Offset, tz_to_seconds(TZ))
              end, TZs).

-define(EPOCHS,
        [
         {1466691033125, 3},
         {1466691033, 0}
        ]).

roundtrip_epoch_test_() ->
    lists:map(fun({Epoch, Precision}) ->
                      ?_assertEqual(Epoch,
                                    to_epoch(from_epoch(Epoch, Precision), Precision))
              end, ?EPOCHS).

-endif.
