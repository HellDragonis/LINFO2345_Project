% Implement a builder that can create blocks from a pending transaction CSV.
% Implement a broadcast step to disseminate a block to all other nodes.
-module(builder).
-export([start/3, create_block/3, broadcast_block/2]).

-record(block, {
    block_number,
    merkle_tree_root,
    builder_address,
    last_block_hash,
    transactions
}).

% Function to start a new builder with a given address
start(Address,  NumValidators, NumNonValidators) ->
    Block = #block{
        block_number = 0,
        merkle_tree_root = "Root",
        builder_address = Address,
        last_block_hash = 0,
        transactions = []
    },
    AtomAddress = list_to_atom(Address),
    Pid = spawn(fun() -> builder_loop(Address, Block) end),
    register(AtomAddress, Pid),
    {ListValidators, ListNonValidators, ListBuilders} = create_node:create_nodes(NumValidators, NumNonValidators),
    UpdatedBuilders = create_node:update_builder_pid(Pid, ListBuilders),
    create_node:display_lists(ListValidators, ListNonValidators, UpdatedBuilders),
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
    BlockNumber =  Block#block.block_number + 1,
    MerkleRoot = merkle_tree:root_hash(Transactions),
    io:format("Merkle Tree : ~n ~s~n", [MerkleRoot]),
    LastBlockHash = case BlockNumber of
        1 -> 0;
        _ -> crypto:hash(sha256, term_to_binary(Block))
        
    end,
    case LastBlockHash of 
        0 -> io:format("Last Block Hash : ~n ~w~n", [LastBlockHash]);
        _ -> io:format("Last Block Hash : ~n ~s~n", [LastBlockHash])
    end, 
    TransactionIDs = case BlockNumber of
    1 -> lists:seq(2, 11);
    _ -> lists:seq((BlockNumber - 1) * 10 + 2, (BlockNumber * 10) + 1)
    end,
    io:format("Transaction Ids : ~n ~w~n", [TransactionIDs]),
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