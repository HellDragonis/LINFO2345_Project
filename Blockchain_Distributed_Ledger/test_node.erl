% test_node.erl
-module(test_node).
-export([run/0]).

run() ->
    Node1 = node:start("Builder_1"),
    Node2 = node:start("Builder_2"),

    % Send a message from NodeA to NodeB
    node:sends_messages(Node1, Node2, 568),

    % Stop the nodes after a short delay to allow for message processing
    timer:sleep(1000),
    Node1 ! stop,
    Node2 ! stop.