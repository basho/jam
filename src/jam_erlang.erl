%% -------------------------------------------------------------------
%%
%% jam_erlang: Convert jam-specific records to and from Erlang's
%%             date/time tuples
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

-module(jam_erlang).

-include("jam_internal.hrl").

-export([to_erlangish_date/1, to_erlangish_time/1, to_erlangish_datetime/1]).
-export([record_to_tuple/1, tuple_to_record/2]).

%% These functions are termed `erlangish' because the resulting tuples
%% may contain `undefined' values, but if not they should be standard
%% Erlang date/time tuples.
%%
%% No timezone or fractional seconds information is included, since
%% Erlang supports none of that as part of the data structures, so if
%% you wish to convert to a different time zone or round fractional
%% seconds do that via the relevant `jam' functions before calling
%% these.

to_erlangish_date(#datetime{date=Date}) ->
    record_to_tuple(Date);
to_erlangish_date(Date) ->
    record_to_tuple(Date).

to_erlangish_time(#datetime{time=Time}) ->
    record_to_tuple(Time);
to_erlangish_time(Time) ->
    record_to_tuple(Time).

to_erlangish_datetime(#datetime{}=DT) ->
    {to_erlangish_date(DT), to_erlangish_time(DT)}.

tuple_to_record(#datetime{}=DT, {Date, Time}) ->
    DT#datetime{date=tuple_to_record(#date{}, Date),
                time=tuple_to_record(#time{}, Time)};
tuple_to_record(#date{}=Date, {Year, Month, Day}) ->
    Date#date{year=Year, month=Month, day=Day};
tuple_to_record(#time{}=Time, {Hour, Minute, Second}) ->
    Time#time{hour=Hour, minute=Minute, second=Second};
tuple_to_record(#fraction{}=Fraction, {Value, Precision}) ->
    Fraction#fraction{value=Value, precision=Precision}.

record_to_tuple(#date{year=Year, month=Month, day=Day}) ->
    {Year, Month, Day};
record_to_tuple(#parsed_calendar{year=Year, month=Month, day=Day}) ->
    {Year, Month, Day};
record_to_tuple(#time{hour=Hour, minute=Minute, second=Second}) ->
    {Hour, Minute, Second};
record_to_tuple(#parsed_time{hour=Hour, minute=Minute, second=Second}) ->
    {Hour, Minute, Second}.
