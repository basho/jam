%% -------------------------------------------------------------------
%%
%% jam_math: Simple date/time math functions against Erlang's
%%           date/time tuples
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
-module(jam_math).
-export([add_time/2, add_date/2]).

add_time({Hour, Minute, Second}, {AddH}) ->
    add_time({Hour, Minute, Second}, {AddH, 0, 0});
add_time({Hour, Minute, Second}, {AddH, AddM}) ->
    add_time({Hour, Minute, Second}, {AddH, AddM, 0});
add_time(Time, Diff) ->
    to_tuple(
      wrap_diff(tuple_to_list(Time), tuple_to_list(Diff),
                [24, 60, 60])).

add_date(Date, NumDays) ->
    calendar:gregorian_days_to_date(
       calendar:date_to_gregorian_days(Date)+NumDays).

to_tuple([H|T]) ->
    {H, list_to_tuple(T)}.

wrap_diff(Time, Diff, Wraps) ->
    wrap_diff_reversed(lists:reverse(Time),
                       lists:reverse(Diff),
                       lists:reverse(Wraps), 0, []).

wrap_diff_reversed([], [], [], Carry, Acc) ->
    [Carry|Acc];
wrap_diff_reversed([Hv|Tv], [Hd|Td], [Hw|Tw], Carry, Acc) ->
    {NewCarry, NewVal} = wrap(Hv + Hd + Carry, Hw, 0),
    wrap_diff_reversed(Tv, Td, Tw, NewCarry, [NewVal|Acc]).

%% Max is exclusive, Min is inclusive. Return value is a tuple:
%% `{Carry, Sum}'
wrap(Sum, Max, Min) when Sum >= Min ->
    {Sum div Max, Sum rem Max};
wrap(Sum, Max, _Min) ->
    AbsSum = abs(Sum),
    {-((AbsSum div Max)+1), Max - (AbsSum rem Max)}.
