-module(create_node).
-export([create_nodes/2, display_lists/3, add_builder/2, update_builder_pid/2]).


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
    create_node:add_builder(Pid, ListBuilders).

display_lists(ListValidators, ListNonValidators, ListBuilders) ->
    io:format("Validators nodes: ~p~n", [ListValidators]),
    io:format("Non-validators nodes: ~p~n", [ListNonValidators]),
    io:format("Builder nodes: ~p~n", [ListBuilders]).
