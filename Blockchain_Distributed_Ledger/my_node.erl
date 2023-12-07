-module(my_node).
-export([start/1, sends_messages/3]).

% Function to start a new node with a given address and register it
start(Address) ->
    AtomAddress = list_to_atom(Address),
    Pid = spawn(fun() -> node_loop(AtomAddress) end),
    register(AtomAddress, Pid),
    Pid.

% Node main loop, it implements the "receives messages" asked
node_loop(Address) ->
    receive
        % Handling incoming messages
        {From, Message} ->
            handle_message(Address, From, Message),
            node_loop(Address);
        % Stop signal
        stop ->
            io:format("~s stopped.~n", [Address])
    end.




% Handling incoming messages, to make the transaction
handle_message(Address, From, Message) ->
    FromName = case erlang:process_info(From, registered_name) of
        {registered_name, RegisteredName} -> RegisteredName;
        _ -> atom_to_list(From)  % If not registered, assume it's already a name
    end.
    % Implement the transaction asked here, and send an acknowledgement (TODO)
    %io:format("Node ~p received message from ~s: ~w~n", [Address, FromName, Message]).


% Function to send a message to another node
sends_messages(From, To, Message) ->
    To ! {From, Message}.
