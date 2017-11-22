
-module(diet).

-export([new/0, new/1, new/2, empty/1, insert/2, merge/2, delete/2, ismember/2]).
%-export([insert/3, delete/3, fold/3]).

%% based on https://web.engr.oregonstate.edu/~erwig/diet/diet.sml.txt


%%%%% ------------------------------------------------------- %%%%%


-type type() :: {diet, tree()}.

-type tree() :: empty | non_empty_tree().
-type non_empty_tree() :: { integer(), integer(), Left :: tree(), Right :: tree() }.


-export_type([type/0]).


%%%%% ------------------------------------------------------- %%%%%


-spec new() -> type().
-spec new( integer() ) -> type().
-spec new( integer(), integer() ) -> type().

new() -> {diet, empty}.

new(Z) -> new(Z, Z).

new(X, Y) when is_integer(X), is_integer(Y), X =< Y ->
    {diet, {X, Y, empty, empty}}.


%%%%% ------------------------------------------------------- %%%%%


-spec empty( type() ) -> boolean().

empty({diet, empty}) -> true;
empty({diet, _}) -> false.


%%%%% ------------------------------------------------------- %%%%%


-spec insert( integer(), type() ) -> type().
%-spec insert( integer(), integer(), type() ) -> type().

insert(Z, {diet, T}) when is_integer(Z) -> tree2type( tree_insert(Z, T) ).

% insert range
%insert(X, Y, {diet, T}) when is_integer(X), is_integer(Y), X =< Y ->
%    tree2type( tree_merge(, T) ).


tree_insert(Z, empty) -> {Z, Z, empty, empty};

tree_insert(Z, {X, Y, L, R} = T) ->
    if
        Z < X   ->
            if
                Z + 1 =:= X -> joinLeft({Z, Y, L, R})
            ;   true        -> {X, Y, tree_insert(Z, L), R}
            end
            
    ;   Z > Y   ->
            if
                Z =:= Y + 1 -> joinRight({X, Z, L, R})
            ;   true        -> {X, Y, L, tree_insert(Z, R)}
            end
            
    ;   true    -> T    % Z in range [X, Y]
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec merge( type(), type() ) -> type().

merge({diet, T1}, {diet, T2}) ->
    tree2type( tree_merge(T1, T2) ).
    
    
% @todo how does this even work?
% T1 = {5, 5, empty, {7, 7, empty, empty}}  -- insert(7, insert(5, new()))
% T2 = {2, 2, empty, {9, 9, empty, empty}}  -- insert(9, insert(2, new()))
% T3 = merge T1, T2
%    = {7, 7, {5, 5, empty, empty}, {2, 2, empty, {9, 9, empty, empty}}}
%
% ismember(2, T3) = false??
%
% TX = insert(9, insert(2, T1))
%    = insert(9, insert(2, {5, 5, empty, {7, 7, empty, empty}} ))
%    = insert(9, {5, 5, {2, 2, empty, empty}, {7, 7, empty, empty}} )
%    = {5, 5, {2, 2, empty, empty}, {7, 7, empty, {9, 9, empty, empty}}}


tree_merge(L, empty) -> L;
tree_merge(empty, R) -> R;
tree_merge(L, R) ->
    {X, Y, L2} = splitMax(L),
    {X, Y, L2, R}.

    
%%%%% ------------------------------------------------------- %%%%%


-spec delete( integer(), type() ) -> type().
%-spec delete( integer(), integer(), type() ) -> type().

delete(Z, {diet, T}) when is_integer(Z) -> tree2type( tree_delete(Z, T) ).

% delete range
%delete(X, Y, {diet, T}) ->


tree_delete(_Z, empty) -> empty;
tree_delete(Z, {X, Y, L, R}) ->
    if
        Z < X   -> {X, Y, tree_delete(Z, L), R}
    ;   Z > Y   -> {X, Y, L, tree_delete(Z, R)}
    ;   Z =:= X ->
            if
                X =:= Y -> merge(L, R)
            ;   true    -> {X + 1, Y, L, R}
            end
            
    ;   Z =:= Y -> {X, Y - 1, L, R}
    ;   true    -> {X, Z -1, L, {Z + 1, Y, empty, R}}   % split the interval
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec ismember( integer(), type() ) -> boolean().

ismember(Z, {diet, T}) when is_integer(Z) ->
    tree_ismember(Z, T).
    
    
tree_ismember(_Z, empty) -> false;
tree_ismember(Z, {X, Y, _L, _R})
    when Z >= X andalso Z =< Y -> true;
    
tree_ismember(Z, {X, Y, L, R}) ->
    if
        Z < X   -> tree_ismember(Z, L)
    ;   X > Y   -> tree_ismember(Z, R)
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec foldr( fun( (integer(), integer(), Acc, Acc) -> Acc ), Acc, tree() ) -> Acc.

foldr( F, Acc, {diet, T}) ->
    tree_foldr(F, Acc, T).
    
    
tree_foldr(_F, Acc, empty) -> Acc;
tree_foldr(F, Acc, {X, Y, L, R}) ->
    F(X, Y, tree_foldr(F, Acc, L), tree_foldr(F, Acc, R)).


%%%%% ------------------------------------------------------- %%%%%


-spec tree2type( tree() ) -> type().
tree2type(T) -> {diet, T}.


%%%%% ------------------------------------------------------- %%%%%


-spec splitMax( non_empty_tree() ) -> { integer(), integer(), tree() }.

splitMax({X, Y, L, empty}) -> {X, Y, L};
splitMax({X, Y, L, R}) ->
    {U, V, R2} = splitMax(R),
    {U, V, {X, Y, L, R2}}.


%%%%% ------------------------------------------------------- %%%%%


-spec splitMin( non_empty_tree() ) -> { integer(), integer(), tree() }.

splitMin({X, Y, empty, R}) -> {X, Y, R};
splitMin({X, Y, L, R}) ->
    {U, V, L2} = splitMin(L),
    {U, V, {X, Y, L2, R}}.


%%%%% ------------------------------------------------------- %%%%%


-spec joinLeft( non_empty_tree() ) -> non_empty_tree().

joinLeft({_, _, empty, _} = T) -> T;
joinLeft({X, Y, L, R}) ->
    {X2, Y2, L2} = splitMax(L),
    if
        Y2 + 1 =:= X    -> {X2, Y, L2, R}
    ;   true            -> {X, Y, L, R}
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec joinRight( non_empty_tree() ) -> non_empty_tree().

joinRight({_, _, _, empty} = T) -> T;
joinRight({X, Y, L, R}) ->
    {X2, Y2, R2} = splitMin(R),
    if
        Y + 1 =:= X2    -> {X, Y2, L, R2}
    ;   true            -> {X, Y, L, R}
    end.

