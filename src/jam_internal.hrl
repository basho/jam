-ifndef(JAM_INT_HRL).
-define(JAM_INT_HRL, included).

%% Seconds between year 0 and 1970. Unsurprisingly, a pretty big number.
-define(GREGORIAN_MAGIC, 62167219200).

-type accuracy() :: 'year'   |
                    'month'  |
                    'day'    |
                    'hour'   |
                    'minute' |
                    'second'.

-type maybe_string() :: string() | 'undefined'.
-type precision() :: non_neg_integer().

-record(parsed_fraction, {
          value :: string()
         }
       ).
-type parsed_fraction() :: #parsed_fraction{}.

-record(parsed_timezone, {
          label :: string(),
          hours :: maybe_string(),
          minutes :: maybe_string()
         }
       ).
-type parsed_timezone() :: #parsed_timezone{}.

-record(parsed_time, {
          hour :: string(),
          minute :: maybe_string(),
          second :: maybe_string(),
          fraction :: parsed_fraction()|'undefined',
          timezone :: parsed_timezone()|'undefined'
         }
       ).
-type parsed_time() :: #parsed_time{}.

-record(parsed_week, {
          year :: string(),
          week :: string(),
          day :: maybe_string()
         }
       ).
-type parsed_week() :: #parsed_week{}.

-record(parsed_ordinal, {
          year :: string(),
          day :: string()
         }
       ).
-type parsed_ordinal() :: #parsed_ordinal{}.

-record(parsed_calendar, {
          year :: string(),
          month :: maybe_string(),
          day :: maybe_string()
         }
       ).
-type parsed_calendar() :: #parsed_calendar{}.

-type parsed_date() :: parsed_calendar() | parsed_ordinal() | parsed_week().

-record(parsed_datetime, {
          date :: parsed_date(),
          time :: #parsed_time{}
         }
       ).
-type parsed_datetime() :: #parsed_datetime{}.

-type parsed_record() :: parsed_time()|parsed_date()|parsed_datetime().

-type year() :: non_neg_integer().
-type date_field() :: pos_integer().
-type time_field() :: non_neg_integer().

-record(timezone, {
          label :: string(),
          hours :: integer(),
          minutes :: integer()
         }
       ).
-type timezone() :: #timezone{}.

-record(fraction, {
          value :: float(),
          precision :: precision()
         }
       ).
-type fraction() :: #fraction{}.

-record(date, {
          year :: year(),
          month :: date_field()|'undefined',
          day :: date_field()|'undefined'
         }
       ).
-type date() :: #date{}.

-record(week, {
          year :: year(),
          week :: date_field(),
          day :: date_field()|'undefined'
         }
       ).
-type week() :: #week{}.

-record(time, {
          hour :: time_field(),
          minute :: time_field()|'undefined',
          second :: time_field()|'undefined',
          fraction :: fraction(),
          timezone :: timezone()
         }
       ).
-type time_record() :: #time{}.

-type date_record() :: #date{}|#week{}.

-record(datetime, {
          date :: date_record()|'undefined',
          time :: time_record()|'undefined'
         }
       ).
-type datetime_record() :: #datetime{}.

-type compiled_record() :: date_record()|time_record()|datetime_record().

-type compilation_error() :: 'incomplete_date'|'incomplete_time'.
-type epoch_error() :: 'missing_time'|'missing_date'|'incomplete_datetime'.

-endif.
