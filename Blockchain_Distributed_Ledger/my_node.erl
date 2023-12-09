-module(my_node).
-export([start/1, sends_messages/3, create_nodes/2, display_lists/3, add_builder/2, update_builder_pid/2]).

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
        {From, {shuffled_list, ShuffledList}} ->
            election_protocol:receive_shuffled_list(ShuffledList),
            node_loop(Address);
        {From, {new_proposer, NewProposerGroup}} ->
            node_loop(Address);
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

create_nodes(NumValidators, NumNonValidators) ->
    {ListValidators, ListBuilders} = create_validators(NumValidators, []),
    ListNonValidators = create_non_validators(NumNonValidators, ListBuilders),
    {ListValidators, ListNonValidators, ListBuilders}.

create_validators(0, Validators) ->
    {Validators, []};
create_validators(NumValidators, Validators) ->
    NodeName = "Validators_" ++ integer_to_list(NumValidators),
    Pid = my_node:start(NodeName),
    create_validators(NumValidators - 1, [Pid | Validators]).

create_non_validators(0, NonValidators) ->
    NonValidators;
create_non_validators(NumNonValidators, NonValidators) ->
    NodeName = "non_Validators_" ++ integer_to_list(NumNonValidators),
    Pid = my_node:start(NodeName),
    create_non_validators(NumNonValidators - 1, [Pid | NonValidators]).

add_builder(BuilderPid, ListBuilders) ->
    [BuilderPid | ListBuilders].

update_builder_pid(Pid, ListBuilders) ->
    add_builder(Pid, ListBuilders).

display_lists(ListValidators, ListNonValidators, ListBuilders) ->
    io:format("Validators nodes: ~p~n", [ListValidators]),
    io:format("Non-validators nodes: ~p~n", [ListNonValidators]),
    io:format("Builder nodes: ~p~n", [ListBuilders]).