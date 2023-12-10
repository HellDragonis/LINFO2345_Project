-module(election_protocol).
-export([init/2, start_election/2, select_proposers/2, broadcast_new_proposers/2, send_to_next_validator/4, receive_shuffled_list/5, send_back/4]).

% Initialization function, called at the bootstrapping
init(ValidatorList, BuilderNode) ->
    clear_log_files(ValidatorList),
    clear_log_file(BuilderNode),
    % Store the initial list of validators and current proposer group head
    Top10Percent = select_top_10_percent(ValidatorList),
    State = #{validators => ValidatorList, proposer_group_head => Top10Percent, builder => BuilderNode},

    % Save the initial state to a file for logging purposes
    log_state(State),
    ParentId = self(),
    NewState = start_election(State, ParentId),
    {NewState, ParentId}.

% Function to initiate a new election
start_election(State, ParentId) ->
    % Get the first node in the lists of Proposer Group
    ProposerGroupHead = maps:get(proposer_group_head, State),
    FirstNode = hd(ProposerGroupHead),

    % Get the Builder Node to send the broadcast
    BuilderNode = maps:get(builder, State),

    % Broadcast the beginning of the election to stop block creation
    broadcast_begin_election(FirstNode, BuilderNode),
    NewState = select_proposers(State, ParentId),
    NewProposerGroup = maps:get(proposer_group_head, NewState),
    broadcast_new_proposers(ProposerGroupHead, NewState),
    broadcast_new_epoch(BuilderNode),
    NewState.


receive_shuffled_list(From, ShuffledList, CurrentValidatorIndex, ProposerGroupHead, ParentId) ->
    %io:format("Received shuffled list: ~p~n", [ShuffledList]),
    %io:format("Current index of validators: ~p~n", [CurrentValidatorIndex]),
    %io:format("Current proposer: ~p~n", [ProposerGroupHead]).
    reshuffle_and_send(CurrentValidatorIndex+1, ShuffledList, ProposerGroupHead, ParentId).

% Function to reshuffle the list and send it to the next validator
reshuffle_and_send(CurrentValidator_index, List, ProposerGroupHead, ParentId) ->
    ShuffledList = shuffle_list(List),
    Last_val = length(ProposerGroupHead),
    NextIndex = length(ProposerGroupHead) + 1,
    case CurrentValidator_index of
        NextIndex ->
            %io:format("Going to send_back ~p ~n", [List]),
            send_back(List, ProposerGroupHead, Last_val, ParentId),
            ShuffledList; % Return ShuffledList when NextIndex matches
        _ ->
            %io:format("New shuffled list is: ~p, sending it to ~p ~n", [ShuffledList, CurrentValidator_index]),
            send_to_next_validator(ShuffledList, CurrentValidator_index, ProposerGroupHead, ParentId),
            ShuffledList % Return ShuffledList when NextIndex doesn't match
    end.






% Function to simulate sending the shuffled list to the next validator
send_to_next_validator(ShuffledList, CurrentValidatorIndex, ProposerGroupHead, ParentId) ->
    case ProposerGroupHead of
        [] ->
            io:format("Proposer group head is empty. Unable to send the shuffled list.~n");
        _ when is_list(ProposerGroupHead) ->
            NextIndex = (CurrentValidatorIndex rem length(ProposerGroupHead)) + 1,
            Current_val = lists:nth(CurrentValidatorIndex, ProposerGroupHead),
            NextValidatorPid = lists:nth(NextIndex, ProposerGroupHead),
            %io:format("Sending shuffled list for ~p to ~p~n", [Current_val, NextValidatorPid]),
            log_operation("Send shuffled list from", Current_val),
            my_node:sends_messages(Current_val, NextValidatorPid, {shuffled_list, ShuffledList, CurrentValidatorIndex, ProposerGroupHead, ParentId});
            
        _ ->
            io:format("Proposer group head is not a list. Unable to send the shuffled list.~n")
    end.

send_back(ShuffledList, ProposerGroupHead, Last_index, ParentId) ->
    CurrentValidatorIndex =1,
    Current_val = lists:nth(CurrentValidatorIndex, ProposerGroupHead),
    Last_val = lists:nth(Last_index, ProposerGroupHead),
    %io:format("Just before send in send back~n ~p~n ~p~n ~p~n", [Current_val, Last_val, self()]),
    my_node:sends_messages(Current_val, Last_val, {last_val, ShuffledList, ParentId}).

% Function to select the proposers and update the state
select_proposers(State, ParentId) ->
    StartTimestamp = erlang:timestamp(),  % Record start time

    AllValidators = maps:get(validators, State),
    ProposerGroupHead = maps:get(proposer_group_head, State),
    ShuffleList = shuffle_list(AllValidators),
    TempShuffledList = reshuffle_and_send(1, ShuffleList, ProposerGroupHead, ParentId),
    ShuffledList = wait_for_shuffled_results(),
    %io:format("Final Shuffled List ~n ~p~n", [ShuffledList]),
    ProposerGroup = select_top_10_percent(ShuffledList),
    NewState = State#{proposer_group_head => ProposerGroup},

    EndTimestamp = erlang:timestamp(),  % Record end time
    TimeDiff = timer:now_diff(EndTimestamp, StartTimestamp),  % Calculate time difference
    io:format("select_proposers took ~p microseconds.~n", [TimeDiff]),

    % Log the election details to the file
    log_state(NewState),
    NewState.


% ---Broadcast function---

% Function to simulate broadcasting the beginning of an election
broadcast_begin_election(ProposerGroupHead, BuilderNode) ->
    BuilderNode ! {begin_election, ProposerGroupHead, BuilderNode},
    log_operation("Broadcasted begin_election from proposer group head", ProposerGroupHead),
    log_operation("Broadcasted begin_election to", BuilderNode).

% Function to broadcast the new proposer group to all nodes
broadcast_new_proposers(NewProposerGroup, State) ->
    ValidatorList = maps:get(validators, State),
    lists:foreach(
        fun(NodePid) ->
            %io:format("Process ~p has sent the following message  ~p to ~p~n", [self(), {NewProposerGroup},NodePid]),
            my_node:sends_messages(self(), NodePid, {new_proposer, NewProposerGroup}),
            log_operation("Sent new proposer group to:", NodePid)
        end,
        ValidatorList
    ).

% Function to simulate broadcasting the beginning of a new epoch
broadcast_new_epoch(BuilderNode) ->
    BuilderNode ! resume_block_creation,
    BuilderNode ! create_block,
    log_operation("Broadcasted new_epoch to", BuilderNode).


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



% Function to get the total number of validators
number_of_validators(State) ->
    ValidatorList = maps:get(validators, State),
    NumberOfValidator = length(ValidatorList),
    NumberOfValidator.

% Function to log the state to a file for each validator
log_state(State) ->
    ValidatorList = maps:get(validators, State),
    lists:foreach(
        fun(Validator) ->
            ValidatorName = case erlang:process_info(Validator, registered_name) of
                {registered_name, RegisteredName} -> atom_to_list(RegisteredName);
                _ -> atom_to_list(Validator)  % If not registered, assume it's already a name
            end,
            LogFileName = "logs/election_log_" ++ ValidatorName ++ ".txt",
            LogData = io_lib:format("~p.~n", [State]),
            file:write_file(LogFileName, LogData, [append]),
            ok
        end,
        ValidatorList).

% Function to log an operation to a file for each validator
log_operation(Operation, Pid) ->
    ValidatorName = case erlang:process_info(Pid, registered_name) of
        {registered_name, RegisteredName} -> atom_to_list(RegisteredName);
        _ -> atom_to_list(Pid)  % If not registered, assume it's already a name
    end,
    LogFileName = "logs/election_log_" ++ ValidatorName ++ ".txt",
    LogData = io_lib:format("~s ~p.~n", [Operation, Pid]),
    file:write_file(LogFileName, LogData, [append]),
    ok.


% Function to clear log files for each validator
clear_log_files(ValidatorList) ->
    lists:foreach(
        fun(Validator) ->
            ValidatorName = case erlang:process_info(Validator, registered_name) of
                {registered_name, RegisteredName} -> atom_to_list(RegisteredName);
                _ -> atom_to_list(Validator)  % If not registered, assume it's already a name
            end,
            LogFileName = "logs/election_log_" ++ ValidatorName ++ ".txt",
            file:delete(LogFileName)
            %io:format("Cleared log file: ~s~n", [LogFileName])
        end,
        ValidatorList).

% Function to clear log files for each validator
clear_log_file(BuilderNode) ->
    BuilderName = case erlang:process_info(BuilderNode, registered_name) of
        {registered_name, RegisteredName} -> atom_to_list(RegisteredName);
        _ -> atom_to_list(BuilderNode)  % If not registered, assume it's already a name
    end,
    LogFileName = "logs/election_log_" ++ BuilderName ++ ".txt",
    file:delete(LogFileName).
    %io:format("Cleared log file: ~s~n", [LogFileName]).



wait_for_shuffled_results() ->
    %io:format("Wait"),
    receive
        % Message indicating validation result from a validator
        {From, {shuffled_result, ShuffledList}} ->
            ShuffledList;
        % Other messages can be handled here
        _ ->
            wait_for_shuffled_results()
    end.