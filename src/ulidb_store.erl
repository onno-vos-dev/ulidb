-module(ulidb_store).

-export([store/2, store/3,
         get/0]).

-define(ETS_TABLE, ulidb_store).

get() ->
  case ets:whereis(?ETS_TABLE) of
    undefined -> {0, <<>>};
    _Tid ->
      [{last_ulidb, Time, Bytes}] = ets:lookup(?ETS_TABLE, last_ulidb),
      {Time, Bytes}
  end.

store(Time, NewBytes) ->
  case ets:whereis(?ETS_TABLE) of
    undefined -> create_table();
    _Tid -> ok
  end,
  true = ets:insert(?ETS_TABLE, [{last_ulidb, Time, NewBytes}]),
  ok.

store(Time, OldBytes, NewBytes) ->
  MS =
    [{{last_ulidb, '_', '$2'},
      [{'=:=', '$2', {const, OldBytes}}],
      [{{last_ulidb, Time, {const, NewBytes}}}]}],
  case ets:select_replace(?ETS_TABLE, MS) of
    1 -> ok;
    0 -> retry
  end.

create_table() ->
  ets:new(?ETS_TABLE,
          [public,
           named_table,
           {write_concurrency, true},
           {read_concurrency, true},
           {decentralized_counters, true}]).
