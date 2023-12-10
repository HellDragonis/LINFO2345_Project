% test_node.erl
-module(test).
-export([run_node/0, run_csv_reader/0, run_builder/0, run_election/0]).

run_node() ->
    Node1 = node:start("Builder_1"),
    Node2 = node:start("Builder_2"),

    % Send a message from NodeA to NodeB
    node:sends_messages(Node1, Node2, 568),

    % Stop the nodes after a short delay to allow for message processing
    timer:sleep(1000),
    Node1 ! stop,
    Node2 ! stop.

run_csv_reader() ->
    csv_reader:read_csv_file("transactions.csv").

run_builder() ->
    builder:start("Builder_1", 4, 5).

run_election() ->
    {Val, NonVal, Builder} = builder:start("Builder_1", 25, 4),
    State = election_protocol:init(Val, Builder),
    State.