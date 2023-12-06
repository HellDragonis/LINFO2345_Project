-module(hash_counter).
-export([get_last_block_hash/0, set_last_block_hash/1, start/0]).

start() ->
    HashTable = ets:new(previous_block_hash, [set, named_table]),
    ets:insert(HashTable, {last_block_hash, <<0:256/native-integer>>}),
    HashTable.

get_last_block_hash() ->
    HashTable = ets:lookup(previous_block_hash, last_block_hash),
    case HashTable of
        [] -> {error, table_not_found};
        [{last_block_hash, Hash}] -> Hash
    end.

set_last_block_hash(NewHash) ->
    HashTable = ets:lookup(previous_block_hash, last_block_hash),
    case HashTable of
        [] -> {error, table_not_found};
        _ -> ets:insert(previous_block_hash, {last_block_hash, NewHash})
    end.