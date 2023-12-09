-module(utility_builder).
-export([build_tree/1, root_hash/1, take_first_n/2, read_transactions/1]).


% Function to build a Merkle tree from a list of data blocks
build_tree(Transactions) ->
    case length(Transactions) of
        % If the Transaction List is empty
        0 -> <<>>;
        % If there is only one element in the Transaction List
        1 -> 
            crypto:hash(sha256, list_to_binary(hd(Transactions)));
        % If there more than one element in the Transaction List
        Len ->
            {Left, Right} = lists:split(Len div 2, Transactions),
            LeftHash = build_tree(Left),
            RightHash = build_tree(Right),
            % Concatenate and hash the children nodes
            crypto:hash(sha256, <<LeftHash/binary, RightHash/binary>>)
    end.



% Function to calculate the root hash of a Merkle tree
root_hash(Transactions) ->
    build_tree(Transactions).



% Take first N element of a list
take_first_n(List, N) when N > 0 ->
    take_first_n(List, N, []).

take_first_n(_, 0, Acc) ->
    lists:reverse(Acc);
take_first_n([], N, Acc) when N > 0 ->
    lists:reverse(Acc);  % Return whatever has been collected so far
take_first_n([H | T], N, Acc) ->
    take_first_n(T, N - 1, [H | Acc]).


read_transactions(FilePath) ->
    Data = function_csv:read_csv_file(FilePath),
    Data.