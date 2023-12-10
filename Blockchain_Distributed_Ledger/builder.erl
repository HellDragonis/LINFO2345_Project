% Implement a builder that can create blocks from a pending transaction CSV.
% Implement a broadcast step to disseminate a block to all other nodes.
-module(builder).
-export([start/3, create_block/5, broadcast_block/3]).

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
    function_csv:clear_csv_file(),
    {ListValidators, ListNonValidators, _} = my_node:create_nodes(NumValidators, NumNonValidators),
    Pid = spawn(fun() -> builder_loop(Address, Block, ListValidators,ListNonValidators) end),
    register(AtomAddress, Pid),
    UpdatedBuilders = my_node:update_builder_pid(Pid, ListBuilders),
    my_node:display_lists(ListValidators, ListNonValidators, UpdatedBuilders).
    %Pid.

% Builder main loop
builder_loop(Address, Block, ListValidators,ListNonValidators) ->
    builder_loop(Address, Block, [], ListValidators,ListNonValidators).

builder_loop(Address, Block, ProcessedTransactions,ListValidators,ListNonValidators) ->
    % Read all transactions from the CSV file
    AllTransactions = read_transactions("transactions.csv"),
    
    case lists:subtract(AllTransactions, ProcessedTransactions) of
        [] ->
             No new transactions, stop the loop
             io:format("No new transactions. Stopping the loop.~n");
        ValidTransactions ->
            io:format("Start loop ~n"),
            %Take the first 10 transactions
            TransactionsForBlock = take_first_n(ValidTransactions, 10),
            io:format("~p~n", [TransactionsForBlock]),
            NewBlock = create_block(Address, TransactionsForBlock, Block,ListValidators,ListNonValidators),
            % Update the list of processed transactions
            NewProcessedTransactions = ProcessedTransactions ++ TransactionsForBlock,
            io:format("End loop ~n"),
            % Continue the loop with the updated processed transactions
            builder_loop(Address, NewBlock, NewProcessedTransactions,ListValidators,ListNonValidators)
    end.

read_transactions(FilePath) ->
    Data = function_csv:read_csv_file(FilePath),
    Data.


% Function to create a new block and broadcast it
create_block(Address, Transactions, Block, ListValidators, ListNonValidators) ->
    BlockNumber =  Block#block.block_number + 1,
    %io:format("Block Number : ~n ~s~n", [MerkleRoot]),
    MerkleRoot = utility_builder:root_hash(Transactions),
    %io:format("Merkle Tree : ~n ~s~n", [MerkleRoot]),
    LastBlockHash = case BlockNumber of
        1 -> 0;
        _ -> crypto:hash(sha256, term_to_binary(Block))
        
    end,
    %case LastBlockHash of 
    %    0 -> io:format("Last Block Hash : ~n ~w~n", [LastBlockHash]);
    %    _ -> io:format("Last Block Hash : ~n ~s~n", [LastBlockHash])
    %end, 
    %io:format("Length of Transactions : ~n ~w~n", [length(Transactions)]),
    TransactionIDs = case {BlockNumber, length(Transactions)} of
        {1, _} -> lists:seq(2, 11);
        {BlockNum, Length} when Length > 1 -> lists:seq((BlockNum - 1) * 10 + 2, (BlockNum - 1)  * 10 + Length + 1);
        {BlockNum, 1} -> [(BlockNum - 1) * 10 + 2];
        _ -> []  % Handle other cases or provide a default value
    end,
    %io:format("Transaction Ids : ~n ~w~n", [TransactionIDs]),
    NewBlock = #block{
        block_number = BlockNumber,
        merkle_tree_root = MerkleRoot,
        builder_address = Address,
        last_block_hash = LastBlockHash,
        transactions = Transactions
    },
    
    BlockData = {BlockNumber, MerkleRoot, Address, LastBlockHash, TransactionIDs},
    function_csv:receive_block_data(BlockData), 
    broadcast_block(ListValidators, ListNonValidators,NewBlock ),
    NewBlock.



% Function to broadcast a block to all nodes
broadcast_block(ListValidators, ListNonValidators, NewBlock) ->
    ReceiverPids = ListValidators ++ ListNonValidators,
    lists:foreach(
        fun(NodePid) ->
            io:format("Process ~p has sent the following message to ~p~n", [self(),NodePid]),
            my_node:sends_messages(self(), NodePid, {NewBlock})
        end,
        ReceiverPids
    ).



% Helper functions
take_first_n(List, N) when N > 0 ->
    take_first_n(List, N, []).

take_first_n(_, 0, Acc) ->
    lists:reverse(Acc);
take_first_n([], N, Acc) when N > 0 ->
    lists:reverse(Acc);  % Return whatever has been collected so far
take_first_n([H | T], N, Acc) ->
    take_first_n(T, N - 1, [H | Acc]).