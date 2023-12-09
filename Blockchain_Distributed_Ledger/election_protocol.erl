-module(election_protocol).
-export([init/2, start_election/1, receive_shuffled_list/1, select_proposers/1, broadcast_new_proposers/1, send_to_next_validator/3]).

% Initialization function, called at the bootstrapping
init(ValidatorList, BuilderNode) ->
    % Store the initial list of validators and current proposer group head
    Top10Percent = select_top_10_percent(ValidatorList),
    State = #{validators => ValidatorList, proposer_group_head => Top10Percent, builder => BuilderNode},

    % Save the initial state to a file for logging purposes
    file:write_file("election_log.txt", io_lib:format("~p.~n", [State])),
    State.

% Function to initiate a new election
start_election(State) ->
    % Get the first node in the lists of Proposer Group
    ProposerGroupHead = maps:get(proposer_group_head, State),
    FirstNode = hd(ProposerGroupHead),

    % Get the Builder Node to send the broadcast
    BuilderNode = maps:get(builder, State),

    % Broadcast the beginning of the election to stop block creation
    broadcast_begin_election(FirstNode, BuilderNode).


% Function to receive the shuffled list from the previous validator
receive_shuffled_list(ShuffledList) ->
    ok.
    % Implement the logic to receive the shuffled list from the previous validator.
    % This function will be called for each validator in the network.

% Function to reshuffle the list and send it to the next validator
reshuffle_and_send(CurrentValidator_index, List, proposer_group_head) ->
    ShuffledList = shuffle_list(List),
<<<<<<< HEAD
    send_to_next_validator(ShuffledList, CurrentValidator_index, proposer_group_head).

=======
    CurrentValidator = 1,
    send_to_next_validator(ShuffledList, CurrentValidator).
>>>>>>> 745deda741920f93b658510e0a7461956a516f43

% Function to simulate sending the shuffled list to the next validator
send_to_next_validator(ShuffledList, CurrentValidatorIndex, ProposerGroupHead) ->
    case ProposerGroupHead of
        [] ->
            io:format("Proposer group head is empty. Unable to send the shuffled list.~n");
        _ when is_list(ProposerGroupHead) ->
            NextIndex = (CurrentValidatorIndex rem length(ProposerGroupHead)) + 1,
            Current_val = lists:nth(CurrentValidatorIndex, ProposerGroupHead),
            NextValidatorPid = lists:nth(NextIndex, ProposerGroupHead),
            io:format("Sending shuffled list for ~p to ~p~n", [Current_val, NextValidatorPid]),
            my_node:sends_messages(Current_val, NextValidatorPid, {ShuffledList});
        _ ->
            io:format("Proposer group head is not a list. Unable to send the shuffled list.~n")
    end.


% Function to select the proposers and update the state
select_proposers(State) ->
    AllValidators = maps:get(validators, State),
    ShuffledList = reshuffle_and_send(AllValidators),
    ProposerGroup = select_top_10_percent(ShuffledList),
    NewState = State#{proposer_group_head => ProposerGroup},
    % Log the election details to the file
    file:write_file("election_log.txt", io_lib:format("~p.~n", [NewState])),
    NewState.


% ---Broadcast function---

% Function to simulate broadcasting the beginning of an election
broadcast_begin_election(ProposerGroupHead, BuilderNode) ->
    BuilderNode ! {begin_election, ProposerGroupHead, BuilderNode}.

% Function to broadcast the new proposer group to all nodes
broadcast_new_proposers(NewProposerGroup) ->
    ok.
    % Implement the logic to broadcast the new proposer group to all nodes.
    % This may involve sending a message to each node in the network.

% Function to simulate broadcasting the beginning of a new epoch
broadcast_new_epoch(NewProposerGroupHead) ->
    ok.
    % Implement the logic to broadcast the new epoch message.
    % This may involve sending a message to the first node in the new proposer group.


% ---Utility functions---

% Function to shuffle a list
shuffle_list(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(N, List) ->
    lists:foldl(fun(_E, Acc) ->
			randomize(Acc)
		end, randomize(List), lists:seq(1, (N - 1))).

randomize(List) ->
    Shuffle1 = lists:map(fun(Elem) -> 
			  {rand:uniform(), Elem}
		  end, List),

    {_, Shuffle2} = lists:unzip(lists:keysort(1, Shuffle1)),
    Shuffle2.

% Function to select the top 10% of the shuffled list
select_top_10_percent(ShuffledList) ->
    Length = length(ShuffledList),
    TenPercent = trunc(Length * 0.1),
    MinCount = max(TenPercent, 1),
    lists:sublist(ShuffledList, 1, MinCount).


% Function to get the number of the next validator
get_next_validator_number(CurrentValidator) ->
    {_, Number} = lists:keyfind(CurrentValidator, 1, 4), % Instead of 4, it is the number of validator
    NextNumber = (Number rem number_of_validators()) + 1,
    NextNumber.

% Function to get the total number of validators
number_of_validators() ->
    4.

