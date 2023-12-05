-module(writer_csv).
-export([write_blocks_to_csv/1]).

% Function to write block information to a CSV file
write_blocks_to_csv(BlockDataList) ->
    FileName = "blocks_data.csv",
    {ok, File} = file:open(FileName, [write]),
    write_block_data(File, BlockDataList),
    file:close(File).

% Function to write block data to the CSV file
write_block_data(_, []) -> ok;
write_block_data(File, [BlockData | RestBlocks]) ->
    {BlockNumber, MerkleRoot, NodeAddress, LastBlockHash, TransactionIDs} = BlockData,
    Line = io_lib:format("~w,~s,~s,~s,~s~n", [BlockNumber, MerkleRoot, NodeAddress, LastBlockHash, TransactionIDs]),
    file:write(File, Line),
    write_block_data(File, RestBlocks).