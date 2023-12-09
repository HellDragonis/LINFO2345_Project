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
    {ListValidators, ListNonValidators, ListBuilders} = my_node:create_nodes(NumValidators, NumNonValidators),
    Pid = spawn(fun() -> builder_loop(Address, Block, ListValidators,ListNonValidators) end),
    register(AtomAddress, Pid),
    UpdatedBuilders = my_node:update_builder_pid(Pid, ListBuilders),
    my_node:display_lists(ListValidators, ListNonValidators, UpdatedBuilders).
    %Pid.

% Builder main loop
builder_loop(Address, Block, ListValidators,ListNonValidators) ->
    builder_loop(Address, Block, [], ListValidators,ListNonValidators).

builder_loop(Address, Block, ProcessedTransactions, ListValidators, ListNonValidators) ->
    % Read all transactions from the CSV file
    AllTransactions = utility_builder:read_transactions("transactions.csv"),
    receive
        % Message indicating the beginning of an election
        {begin_election, ProposerGroupHead, BuilderNode} ->
            io:format("Received begin_election message. Stopping block creation.~n"),
            % Wait for a resume message before creating blocks again
            wait_for_resume(Address, Block, ProcessedTransactions, ListValidators, ListNonValidators);

        % Message indicating block creation
        create_block ->
            NumBlocks = 10,
            {NewBlock, NewProcessedTransactions} = create_blocks(Address, Block, AllTransactions, ProcessedTransactions, ListValidators, ListNonValidators, NumBlocks),
            builder_loop(Address, NewBlock, NewProcessedTransactions,ListValidators,ListNonValidators);

        % Other messages (if any) can be handled here
        _ ->
            builder_loop(Address, Block, [], ListValidators, ListNonValidators)
    end.

% Function to wait for a resume message before creating blocks again
wait_for_resume(Address, Block, ProcessedTransactions, ListValidators, ListNonValidators) ->
    receive
        % Message indicating the resumption of block creation
        resume_block_creation ->
            io:format("Resuming block creation.~n"),
            % Continue the loop with an empty list of processed transactions
            builder_loop(Address, Block, ProcessedTransactions, ListValidators, ListNonValidators);
        % Other messages (if any) can be handled here
        _ ->
            wait_for_resume(Address, Block, ProcessedTransactions, ListValidators, ListNonValidators)
    end.

create_blocks(_, NewBlock, _, NewProcessedTransactions, _, _, 0) ->
    {NewBlock, NewProcessedTransactions};
create_blocks(Address, Block, AllTransactions, ProcessedTransactions, ListValidators, ListNonValidators, NumBlocks) ->
    case lists:subtract(AllTransactions, ProcessedTransactions) of
        [] ->
            % No new transactions, stop the loop
            io:format("No new transactions. Stopping the loop.~n"),
        create_blocks(Address, Block, AllTransactions, ProcessedTransactions, ListValidators, ListNonValidators, 0);
        ValidTransactions ->
            io:format("Start loop ~n"),
            % Take the first 10 transactions
            TransactionsForBlock = utility_builder:take_first_n(ValidTransactions, 10),
            io:format("~p~n", [TransactionsForBlock]),
            NewBlock = create_block(Address, TransactionsForBlock, Block, ListValidators, ListNonValidators),
            % Update the list of processed transactions
            NewProcessedTransactions = ProcessedTransactions ++ TransactionsForBlock,
            io:format("End loop ~n"),
            % Continue the loop with the updated processed transactions
            create_blocks(Address, NewBlock, AllTransactions, NewProcessedTransactions, ListValidators, ListNonValidators, NumBlocks - 1)
    end.


% Function to create a new block and broadcast it
create_block(Address, Transactions, Block, ListValidators, ListNonValidators) ->
    BlockNumber =  Block#block.block_number + 1,
    io:format("Block Number : ~n ~p~n", [BlockNumber]),
    MerkleRoot = utility_builder:root_hash(Transactions),
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
        _ when length(Transactions) > 1 -> lists:seq((BlockNumber - 1) * 10 + 2, (BlockNumber - 1)  * 10 + length(Transactions) + 1);
        _ when length(Transactions) == 1 -> [(BlockNumber - 1) * 10 + 2]
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
    function_csv:receive_block_data(BlockData), 
    broadcast_block(ListValidators, ListNonValidators,NewBlock ),
    NewBlock.


% Function to broadcast a block to all nodes
broadcast_block(ListValidators, ListNonValidators, NewBlock) ->
    ReceiverPids = ListValidators ++ ListNonValidators,
    lists:foreach(
        fun(NodePid) ->
            %io:format("Process ~p has sent the following message  ~p to ~p~n", [self(), {NewBlock},NodePid]),
            my_node:sends_messages(self(), NodePid, {NewBlock})
        end,
        ReceiverPids
    ).