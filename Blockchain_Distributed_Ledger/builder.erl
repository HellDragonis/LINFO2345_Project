% Implement a builder that can create blocks from a pending transaction CSV.
% Implement a broadcast step to disseminate a block to all other nodes.
-module(builder).
-export([start/1, create_block/1, broadcast_block/2]).
-include("csv_reader.erl").

% Function to start a new builder with a given address
start(Address) ->
    Pid = spawn(fun() -> builder_loop(Address) end),
    register(Address, Pid),
    Pid.

% Builder main loop
builder_loop(Address) ->
    builder_loop(Address, []).

builder_loop(Address, ProcessedTransactions) ->
    % Read all transactions from the CSV file
    AllTransactions = read_transactions("transactions.csv"),
    % Exclude transactions that have already been processed
    ValidTransactions = lists:subtract(AllTransactions, ProcessedTransactions),
    % Take the first 10 transactions
    TransactionsForBlock = lists:sublist(ValidTransactions, 1, 10),
    
    create_block(Address, TransactionsForBlock),
    % Update the list of processed transactions
    NewProcessedTransactions = ProcessedTransactions ++ TransactionsForBlock,
    
    % Continue the loop with the updated processed transactions
    builder_loop(Address, NewProcessedTransactions).


read_transactions(FilePath) ->
    Data = csv_reader:read_csv_file(FilePath),
    Data.


% Function to create a new block and broadcast it
create_block(Address, Transactions) ->
    Block = #block{
        block_number = get_next_block_number(),
        merkle_tree_root = calculate_merkle_tree_root(Transactions),
        builder_address = Address,
        last_block_hash = get_last_block_hash(),
        transactions = Transactions
    },
    broadcast_block(Address, Block).

% Function to broadcast a block to all nodes
broadcast_block(Address, Block) ->
    Nodes = get_all_nodes(),
    lists:foreach(fun(Node) -> node:sends_messages(Node, {block, Block}) end, Nodes).

% Placeholder functions for future implementation
get_next_block_number() ->
    % Implement logic to get the next block number


calculate_merkle_tree_root(Transactions) ->
    % Implement Merkle tree calculation logic


get_last_block_hash() ->
    % Implement logic to get the hash of the last published block
    


-record(block, {
    block_number,
    merkle_tree_root,
    builder_address,
    last_block_hash,
    transactions
}).