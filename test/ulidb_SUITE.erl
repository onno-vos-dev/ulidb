-module(ulidb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).
-export([generate_0/1,
         generate_1/1,
         generate_binary_0/1,
         generate_binary_1/1,
         sorted_ulids_within_same_millisecond/1]).

%%====================================================================
%% Common Test callbacks
%%====================================================================
suite() -> [{timetrap, {minutes, 1}}].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, _Config) -> ok.

all() ->
  [generate_0,
   generate_1,
   generate_binary_0,
   generate_binary_1,
   sorted_ulids_within_same_millisecond].

%%====================================================================
%% Tests
%%====================================================================
generate_0(_Config) ->
  Time = erlang:system_time(millisecond),
  Ulid = ulidb:generate(),
  ?assertEqual(Ulid,
               ulidb:encode(
                 ulidb:decode(Ulid))),
  ExtractedTime = ulidb:extract_timestamp(Ulid),
  ?assert(Time =< ExtractedTime),
  Uuid = ulidb:to_uuid(Ulid),
  ?assertEqual(Ulid,
               ulidb:encode(
                 ulidb:from_uuid(Uuid))).

generate_1(_Config) ->
  Time = erlang:system_time(millisecond),
  Ulid = ulidb:generate(Time),
  ?assertEqual(Ulid,
               ulidb:encode(
                 ulidb:decode(Ulid))),
  ExtractedTime = ulidb:extract_timestamp(Ulid),
  ?assertEqual(Time, ExtractedTime),
  Uuid = ulidb:to_uuid(Ulid),
  ?assertEqual(Ulid,
               ulidb:encode(
                 ulidb:from_uuid(Uuid))).

generate_binary_0(_Config) ->
  Time = erlang:system_time(millisecond),
  UlidBin = ulidb:generate_binary(),
  ?assertEqual(UlidBin,
               ulidb:decode(
                 ulidb:encode(UlidBin))),
  ExtractedTime = ulidb:extract_timestamp(UlidBin),
  ?assert(Time =< ExtractedTime),
  Uuid = ulidb:to_uuid(UlidBin),
  ?assertEqual(UlidBin, ulidb:from_uuid(Uuid)).

generate_binary_1(_Config) ->
  Time = erlang:system_time(millisecond),
  UlidBin = ulidb:generate_binary(Time),
  ?assertEqual(UlidBin,
               ulidb:decode(
                 ulidb:encode(UlidBin))),
  ExtractedTime = ulidb:extract_timestamp(UlidBin),
  ?assertEqual(Time, ExtractedTime),
  Uuid = ulidb:to_uuid(UlidBin),
  ?assertEqual(UlidBin, ulidb:from_uuid(Uuid)).

sorted_ulids_within_same_millisecond(_Config) ->
  Time = erlang:system_time(millisecond),
  Ulids = [ulidb:generate(Time) || _ <- lists:seq(1, 1_000_000)],
  ?assertEqual(Ulids, lists:usort(Ulids)).
