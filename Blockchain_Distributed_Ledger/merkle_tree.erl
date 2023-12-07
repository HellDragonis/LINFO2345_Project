-module(merkle_tree).
-export([build_tree/1, root_hash/1]).


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