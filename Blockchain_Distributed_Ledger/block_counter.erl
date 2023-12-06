-module(block_counter).
-export([get_next_block_number/0, start/0]).

% Using an ETS table to maintain the block count
start() ->
    CountTable = ets:new(block_count, [set, named_table]),
    ets:insert(CountTable, {block_counter, 0}),
    CountTable.

get_next_block_number() ->
    CountTable = ets:lookup(block_count, block_counter),
    case CountTable of
        [] -> {error, table_not_found};
        [{block_counter, Count}] ->
            NextBlockNumber = Count + 1,
            ets:insert(block_count, {block_counter, NextBlockNumber}),
            NextBlockNumber
    end.