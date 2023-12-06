% Implement a builder that can create blocks from a pending transaction CSV.
% Implement a broadcast step to disseminate a block to all other nodes.
-module(builder).
-export([start/1, create_block/2, broadcast_block/2]).


-record(block, {
    block_number,
    merkle_tree_root,
    builder_address,
    last_block_hash,
    transactions
}).

% Function to start a new builder with a given address
start(Address) ->
    AtomAddress = list_to_atom(Address),
    Pid = spawn(fun() -> builder_loop(Address) end),
    register(AtomAddress, Pid),
    block_counter:start(),
    Pid.

% Builder main loop
builder_loop(Address) ->
    builder_loop(Address, []).

builder_loop(Address, ProcessedTransactions) ->
    % Read all transactions from the CSV file
    AllTransactions = read_transactions("transactions.csv"),
    
    case lists:subtract(AllTransactions, ProcessedTransactions) of
        [] ->
            % No new transactions, stop the loop
            io:format("No new transactions. Stopping the loop.~n");
        ValidTransactions ->
            io:format("Start loop ~n"),
            % Take the first 10 transactions
            TransactionsForBlock = take_first_n(ValidTransactions, 10),
            io:format("~p~n", [TransactionsForBlock]),
            create_block(Address, TransactionsForBlock),
            % Update the list of processed transactions
            NewProcessedTransactions = ProcessedTransactions ++ TransactionsForBlock,
            io:format("End loop ~n"),
            % Continue the loop with the updated processed transactions
            builder_loop(Address, NewProcessedTransactions)
    end.

read_transactions(FilePath) ->
    Data = csv_reader:read_csv_file(FilePath),
    Data.


% Function to create a new block and broadcast it
create_block(Address, Transactions) ->
    BlockNumber = block_counter:get_next_block_number(),
    MerkleRoot = calculate_merkle_tree_root(Transactions),
    LastBlockHash = get_last_block_hash(),
    Block = #block{
        block_number = BlockNumber,
        merkle_tree_root = MerkleRoot,
        builder_address = Address,
        last_block_hash = LastBlockHash,
        transactions = Transactions
    },
    
    BlockData = {BlockNumber, MerkleRoot, Address, LastBlockHash, Transactions},
    writer_csv:receive_block_data(BlockData).



% Function to broadcast a block to all nodes
broadcast_block(Address, Block) ->
    ok.
    %Nodes = get_all_nodes(),
    %lists:foreach(fun(Node) -> node:sends_messages(Node, {block, Block}) end, Nodes).


calculate_merkle_tree_root(Transactions) ->
    1.
    % Implement Merkle tree calculation logic
    
    
get_last_block_hash() ->
    2.
    % Implement logic to get the hash of the last published block    


% Helper functions
take_first_n(List, N) when N > 0 ->
    take_first_n(List, N, []).

take_first_n(_, 0, Acc) ->
    lists:reverse(Acc);
take_first_n([H | T], N, Acc) ->
    take_first_n(T, N - 1, [H | Acc]).