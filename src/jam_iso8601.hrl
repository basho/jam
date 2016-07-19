-ifndef(JAM_ISO8601_HRL).
-define(JAM_ISO8601_HRL, included).

-define(CALENDAR_DATE,
        "(?<YEAR>\\d{4})((?<DSEP>-?)(?<MONTH>\\d{2}))?(\\k'DSEP'(?<DAY>\\d{2}))?").

%% XXX We recognize week dates but do not yet support them properly.
-define(WEEK_DATE,
        "(?<YEAR>\\d{4})((?<DSEP>-?)(W(?<WEEK>\\d{2})))(\\k'DSEP'(?<DAY>\\d))?").

-define(ORDINAL_DATE,
        "(?<YEAR>\\d{4})(?<DSEP>-?)(?<DAY>\\d{3})").

%% Full dates are required for datetime strings
-define(CALENDAR_DATE_FULL,
        "(?<YEAR>\\d{4})((?<DSEP>-?)(?<MONTH>\\d{2}))(\\k'DSEP'(?<DAY>\\d{2}))").

-define(WEEK_DATE_FULL,
        "(?<YEAR>\\d{4})((?<DSEP>-?)(W(?<WEEK>\\d{2})))(\\k'DSEP'(?<DAY>\\d))").

-define(ORDINAL_DATE_FULL, ?ORDINAL_DATE).

-define(TIMEZONE, "(?<TZ>(Z|(?<TZH>[+-]\\d{2})(:?(?<TZM>\\d{2}))?))").

%% The TIME regex is overly permissive.
%%
%% The standard mandates that either separators are used throughout a
%% date/time/time zone string, or they are not used at all.
%%
%% To enforce that in the time zone section is problematic because of
%% fractional hours. It's entirely possible that we have never "seen"
%% a `TSEP' value by the time we reach the time zone, and a match
%% against it would fail.
%%
%% So: 1215.123+0500 is fine, as is 12:15.123+05:00
%% Also fine: 12.123+05
%% Would fail if we check `TSEP' in the time zone pattern:
%%    12.123+0500 and 12.123+05:00

-define(TIME,
        "(?<HOUR>\\d{2})((?<TSEP>:?)(?<MINUTE>\\d{2}))?(\\k'TSEP'(?<SECOND>\\d{2}))?(([.,])(?<FRACTIONAL>\\d+))?" ++ ?TIMEZONE ++ "?").

%% XXX Be sure to document we have abandoned making certain that
%% separators are used consistently between date and time. Must be
%% used internally consistently within a date or time (except the time
%% zone issue noted above).

-define(TTIME, "T?" ++ ?TIME).

-define(ANCHOR(RE),
        "^\\s*" ++ RE ++ "\\s*$").

-define(CALENDAR_TIME(SEP), ?ANCHOR(?CALENDAR_DATE_FULL ++ SEP ++ ?TIME)).
-define(WEEK_TIME(SEP), ?ANCHOR(?WEEK_DATE_FULL ++ SEP ++ ?TIME)).
-define(ORDINAL_TIME(SEP), ?ANCHOR(?ORDINAL_DATE_FULL ++ SEP ++ ?TIME)).

-endif.
