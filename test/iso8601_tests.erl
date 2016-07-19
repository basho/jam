%%%-------------------------------------------------------------------
%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2016, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 23 Jun 2016 by John Daily <jd@epep.us>
%%%-------------------------------------------------------------------
-module(iso8601_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(DATES_NOMATCH,
        [
         "2016-0623",
         "201606-23",
         "2016:06:23",
         "2016.06.23"
        ]).

-define(DATES,
        [
         {"2016", 1451606400},
         {"2016-06", 1464739200},
         {"20160623", 1466640000},
         {"2016-06-23", 1466640000},
         {"2016-175", 1466640000},
         {"2016175", 1466640000}
        ]).

%% Translate into number of seconds into the day
-define(TIMES,
        [
         {"14", 50400},
         {"14:10", 51000},
         {"T1410", 51000},
         {"14:10:33", 51033},
         {"141033", 51033},
         {"14.17583", 51033},
         {"14,17583", 51033},
         {"1410.55", 51033},
         {"14:10.55", 51033}
        ]).

%% These mappings assume UTC.
-define(DATETIMES,
        [
         {"20160623T14", 1466690400},
         {"2016-06-23T14:10", 1466691000},
         {"20160623T1410", 1466691000},
         {"2016-06-23T14:10:33", 1466691033},
         {"20160623T141033", 1466691033},
         {"2016-06-23T14.17583", 1466691033},
         {"20160623T14,17583", 1466691033},
         {"20160623T1410.55", 1466691033},
         {"2016-06-23T14:10.55", 1466691033}
        ]).

%% Have to be converted via to_epoch with 1000 as the optional
%% argument for millisecond accuracy
-define(FRACTIMES,
        [
         {"2016-06-23T14:10:33.1254", 1466691033125},
         {"20160623T141033,1254", 1466691033125}
        ]).

-define(TZCONVERSIONS,
        [
         {"20160301T11:14:33+06:30", "-08:15",
          {{2016, 2, 29}, {20, 29, 33}}},
         {"20160101T11:14:33+06:30", "-08:15",
          {{2015, 12, 31}, {20, 29, 33}}},
         {"20160101T03:14:33+06:30", "+12:15",
          {{2016, 1, 1}, {8, 59, 33}}}
        ]).

-define(EXTENDED_STRINGS,
        [
         "2016-06-23T14:10",
         "2016-06-23T14:10:33",
         "2016-06-23T14:10:33Z",
         "2016-06-23T14:10:33+05:00",
         "2016-06-23T14:10:33-11:30",
         "2016-06-23T00:00:05.023",
         "14:10:59",
         "2016-06",
         "2016-06-23",
         "2016"
        ]).

-define(BASIC_STRINGS,
        [
         "20160623T141033",
         "20160623T141033+0645",
         "20160623",
         "2016"
        ]).

%% Seconds between year 0 and 1970. Unsurprisingly, a pretty big number.
-define(GREGORIAN_MAGIC, 62167219200).

to_string_test_() ->
    lists:map(fun(Str) -> ?_assertEqual(Str, jam_iso8601:to_string(jam:process(jam_iso8601:parse(Str)))) end,
              ?EXTENDED_STRINGS)
        ++
    lists:map(fun(Str) -> ?_assertEqual(Str, jam_iso8601:to_string(jam:process(jam_iso8601:parse(Str)), [{format, basic}])) end,
              ?BASIC_STRINGS).

non_roundtrip_string_test() ->
    %% Various string generation properties we wish to test:

    %%   `{t_prefix, true}' is dropped during processing of a datetime
    %%   lest we end up with a space *and* a T separator

    %%   `{datetime_separator, space|t}' works

    %%   `{decimal_mark, comma|period}' works

    %%   `{z, true|false}' works
    ?assertEqual("2016-06-23 23:59:59,123-05:00",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("20160623T235959.123-0500")), [{datetime_separator, space}, {t_prefix, true}, {decimal_mark, comma}])),
    ?assertEqual("2016-06-23 23:59:59,123+00:00",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("20160623T235959.123Z")), [{datetime_separator, space}, {t_prefix, true}, {decimal_mark, comma}, {z, false}])),
    ?assertEqual("2016-06-23 23:59:59.123+00:00",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("20160623T235959.123Z")), [{datetime_separator, space}, {t_prefix, true}, {decimal_mark, period}, {z, false}])),
    ?assertEqual("2016-06-23T23:30:00Z",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("20160623T23.500Z")), [{datetime_separator, t}, {t_prefix, true}, {decimal_mark, comma},{z, true}])),
    ?assertEqual("2016-06-23T23Z",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("20160623T23Z")), [{datetime_separator, t}, {t_prefix, true}, {decimal_mark, comma},{z, true}])),
    ?assertEqual("T2300Z",
                 jam_iso8601:to_string(
                   jam:process(jam_iso8601:parse("23:00+00")), [{format, basic}, {datetime_separator, t}, {t_prefix, true}, {decimal_mark, comma},{z, true}])),
    ?assertEqual("2016-06-23T23:00Z",
                 jam_iso8601:to_string(
                   jam:process(
                     jam_iso8601:parse("20160623T23Z"),
                     [{target_accuracy, minute}]),
                   [{datetime_separator, t}, {t_prefix, true}, {decimal_mark, comma},{z, true}])).




map_tz(Str, TZ) ->
    {_Bump, NewDateTime} = jam:convert_tz(jam:process(jam_iso8601:parse(Str)), TZ),
    jam_erlang:to_datetime(NewDateTime).

tzconversions_test_() ->
    lists:map(fun({DateTime, NewTZ, Result}) ->
                      ?_assertEqual(Result, map_tz(DateTime, NewTZ))
              end, ?TZCONVERSIONS).

fractional_test_() ->
    lists:map(fun({Date, Result}) ->
                      ?_assertEqual(Result, map_frac(Date))
              end, ?FRACTIMES).

dates_test_() ->
    %% For performance, we can compile the ISO 8601 regular
    %% expressions. Use that functionality for this test.
    State = jam_iso8601:init(),
    lists:map(fun({Date, Result}) ->
                      ?_assertEqual(Result, map_date(State, Date))
              end, ?DATES).

times_test_() ->
    lists:map(fun({Time, Result}) ->
                      ?_assertEqual(Result, map_utc_time(Time))
              end, ?TIMES).

datetimes_test_() ->
    lists:map(fun({Datetime, Result}) ->
                      ?_assertEqual(Result, map_utc_datetime(Datetime ++ "Z"))
              end, ?DATETIMES).

timezones_test_() ->
    lists:map(fun({Datetime, Result}) ->
                      ?_assertEqual(Result, map_utc_datetime(Datetime ++ "Z"))
              end, ?DATETIMES)
        ++
    lists:map(fun({Datetime, Result}) ->
                      %% EQC!
                      TZ = 7,
                      TZStr = case string:rchr(Datetime, $:) of
                                  0 ->
                                      %% No separators
                                      "+0700";
                                  _ ->
                                      "+07:00"
                              end,
                      ?_assertEqual(add_tz(Result, TZ), map_utc_datetime(Datetime ++ TZStr))
             end, ?DATETIMES)
        ++
    lists:map(fun({Datetime, Result}) ->
                      %% EQC!
                      TZ = 12.5,
                      TZStr = case string:rchr(Datetime, $:) of
                                  0 ->
                                      %% No separators
                                      "+1230";
                                  _ ->
                                      "+12:30"
                              end,
                      ?_assertEqual(add_tz(Result, TZ), map_utc_datetime(Datetime ++ TZStr))
             end, ?DATETIMES)
        ++
    lists:map(fun({Datetime, Result}) ->
                      %% EQC!
                      TZ = -3.5,
                      TZStr = case string:rchr(Datetime, $:) of
                                  0 ->
                                      %% No separators
                                      "-0330";
                                  _ ->
                                      "-03:30"
                              end,
                      ?_assertEqual(add_tz(Result, TZ), map_utc_datetime(Datetime ++ TZStr))
              end, ?DATETIMES).

map_date(State, Str) ->
    jam:to_epoch(jam:process(jam_iso8601:parse(State, Str), [{target_accuracy, second}])).

map_frac(Str) ->
    jam:to_epoch(jam:process(jam_iso8601:parse(Str)), 1000).

map_utc_time(Str) ->
    %% Arbitrary date
    Date = {2001, 12, 03},
    Time = jam_iso8601:parse(Str),
    ProcessedTime = jam:process(Time, [{target_accuracy, second}]),

    %% If this results in a changed date, we need to add another test
    %% utility function to increment our arbitrary date
    {0, NewTime} =
        jam:round_fractional_seconds(ProcessedTime),
    RoundedTime = jam_erlang:to_time(NewTime),
    universal_datetime_TO_utc_seconds({Date, RoundedTime}) -
        universal_datetime_TO_utc_seconds({Date, {0, 0, 0}}).

map_datetime(Str) ->
    Processed = jam:process(jam_iso8601:parse(Str), [{target_accuracy, second}]),
    {_Bump, Rounded} = jam:round_fractional_seconds(Processed),
    jam:to_epoch(Rounded).

map_utc_datetime(Str) ->
    Processed = jam:process(jam_iso8601:parse(Str), [{target_accuracy, second}]),
    {_Bump, DateTime} = jam:round_fractional_seconds(Processed),
    {_UTCBump, UTCDateTime} = jam:convert_tz(DateTime, "+00:00"),
    jam:to_epoch(UTCDateTime).

add_tz(UTC, TZ) ->
    UTC - trunc(3600 * TZ).

universal_datetime_TO_utc_seconds(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime)
        - ?GREGORIAN_MAGIC.
