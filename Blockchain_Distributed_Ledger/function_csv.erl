-module(function_csv).
-export([read_csv_file/1, print_csv_data/1, write_blocks_to_csv/1, receive_block_data/1, clear_csv_file/0]).


read_csv_file(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    {ok, _Header} = file:read_line(Device), % Read and discard the header
    Data = read_lines(Device, []),
    file:close(Device),
    Data.

read_lines(Device, Acc) ->
    case file:read_line(Device) of
        {ok, Header} when Header =:= "sender,receiver,money\n" ->  % Skip the first line (header)
            read_lines(Device, Acc);
        {ok, Line} ->
            % Split the line by comma (CSV delimiter)
            Columns = string:tokens(Line, ",\n"),
            read_lines(Device, [Columns | Acc]);
        eof ->
            lists:reverse(Acc)
    end.  

print_csv_data([]) ->
    ok;
print_csv_data([Row | Rows]) ->
    io:format("~p~n", [Row]),
    print_csv_data(Rows).

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

receive_block_data(BlockData) ->
    {BlockNumber, MerkleRoot, NodeAddress, LastBlockHash, TransactionIDs} = BlockData,
    % Convert TransactionIDs list to a comma-separated string
    TransactionIDsStr = string:join([integer_to_list(ID) || ID <- TransactionIDs], ","),
    FileName = "blocks_data.csv",
    {ok, File} = file:open(FileName, [append]),
    Line = io_lib:format("~w,~w,~s,~w,~s~n", [BlockNumber, MerkleRoot, NodeAddress, LastBlockHash, TransactionIDsStr]),
    file:write(File, Line),
    file:close(File).

clear_csv_file() ->
    FileName = "blocks_data.csv",
    {ok, File} = file:open(FileName, [write]),
    file:write(File, ""),  % Writing an empty string to the file clears its content
    file:close(File).