% Implement a builder that can create blocks from a pending transaction CSV.
% Implement a broadcast step to disseminate a block to all other nodes.
-module(builder).
-export([start/1, create_block/3, broadcast_block/2]).

-record(block, {
    block_number,
    merkle_tree_root,
    builder_address,
    last_block_hash,
    transactions
}).

% Function to start a new builder with a given address
start(Address) ->
    Block = #block{
        block_number = 0,
        merkle_tree_root = "Root",
        builder_address = "Builder_0",
        last_block_hash = null,
        transactions = []
    },
    AtomAddress = list_to_atom(Address),
    Pid = spawn(fun() -> builder_loop(Address, Block) end),
    register(AtomAddress, Pid),
    Pid.

% Builder main loop
builder_loop(Address, Block) ->
    builder_loop(Address, Block, []).

builder_loop(Address, Block, ProcessedTransactions) ->
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
            NewBlock = create_block(Address, TransactionsForBlock, Block),
            % Update the list of processed transactions
            NewProcessedTransactions = ProcessedTransactions ++ TransactionsForBlock,
            io:format("End loop ~n"),
            % Continue the loop with the updated processed transactions
            builder_loop(Address, NewBlock, NewProcessedTransactions)
    end.

read_transactions(FilePath) ->
    Data = csv_reader:read_csv_file(FilePath),
    Data.


% Function to create a new block and broadcast it
create_block(Address, Transactions, Block) ->
    io:format("Create Block with the transactions : ~n ~p~n", [Transactions]),
    BlockNumber =  Block#block.block_number + 1,
    io:format("Block Number : ~n ~p~n", [BlockNumber]),
    MerkleRoot =  1,%merkle_tree:root_hash(Transactions),
    LastBlockHash = 2, %hash_counter:get_last_block_hash(),
    io:format("Last Block Hash: ~p~n", [LastBlockHash]),
    TransactionIDs = [1,2,3,4,5,6,7,8,9,10],
    NewBlock = #block{
        block_number = BlockNumber,
        merkle_tree_root = MerkleRoot,
        builder_address = Address,
        last_block_hash = LastBlockHash,
        transactions = Transactions
    },
    
    BlockData = {BlockNumber, MerkleRoot, Address, LastBlockHash, TransactionIDs},
    writer_csv:receive_block_data(BlockData), 
    NewBlock.



% Function to broadcast a block to all nodes
broadcast_block(Address, Block) ->
    ok.
    %Nodes = get_all_nodes(),
    %lists:foreach(fun(Node) -> node:sends_messages(Node, {block, Block}) end, Nodes).



% Helper functions
take_first_n(List, N) when N > 0 ->
    take_first_n(List, N, []).

take_first_n(_, 0, Acc) ->
    lists:reverse(Acc);
take_first_n([H | T], N, Acc) ->
    take_first_n(T, N - 1, [H | Acc]).