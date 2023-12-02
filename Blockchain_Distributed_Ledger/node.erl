% Implement a node that sends and receives messages.

-module(node).
-export([start/1, sends_messages/2]).

% Function to start a new node with a given address and register it
start(Address) ->
    Pid = spawn(fun() -> node_loop(Address) end),
    register(Address, Pid),
    Pid.


% Node main loop, it implements the "receives messages" asked
node_loop(Address) ->
    receive
        % Handling incoming messages
        {From, Message} ->
            handle_message(From, Message),
            node_loop(Address);
        % Stop signal
        stop ->
            io:format("~s stopped.~n", [Address])
    end.


% Handling incoming messages, to make the transaction
handle_message(From, Message) ->
    % Implement the transaction asked here, and send an acknowledgement (TODO)
    io:format("Node ~s received message from ~s: ~p~n", [Address, From, Message]).


% Function to send a message to another node
sends_messages(To, Message) ->
    To ! {self(), Message},
    % Acknowledgement to be sure that the message has been sent
    receive
        {To, _} -> ok
    after 5000 ->
        io:format("Timeout sending message to ~s~n", [To])
    end.