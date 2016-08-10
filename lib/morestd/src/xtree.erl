
-module(xtree).

-export([delete_all/2, foreach/2, fold/3, filter/2, delete_if/2]).



%%%%% ------------------------------------------------------- %%%%%


-spec delete_all( [K], gb_trees:tree(K, V) ) -> gb_trees:tree(K, V).

delete_all(Keys, Tree) ->
    % TODO iterate through tree once removing K's
    lists:foldl(
            fun(X, T) ->
                gb_trees:delete_any(X, T)
            end
        , Tree, Keys).


%%%%% ------------------------------------------------------- %%%%%


-spec foreach( fun((K, V) -> V), gb_trees:tree(K, V) ) -> gb_trees:tree(K, V).

foreach(_Pred, Tree) ->
    Tree.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec fold( fun(({K, V}, A) -> A), A, gb_trees:tree(K, V) ) -> A.

fold(Fun, Acc, Tree) ->
    lists:foldl(Fun, Acc, gb_trees:to_list(Tree) ).


%%%%% ------------------------------------------------------- %%%%%


-spec filter( fun((K, V) -> boolean()), gb_trees:tree(K, V) ) -> gb_trees:tree(K, V).

filter(Pred, Tree)
        when is_function(Pred, 2)  ->
        
    NewTree = lists:filter(
                    fun({Key, Value}) ->
                        Pred(Key, Value)
                    end
                , gb_trees:to_list(Tree) ),
    gb_trees:from_orddict(NewTree).
    

%%%%% ------------------------------------------------------- %%%%%


-spec delete_if( fun((K, V) -> boolean()), gb_trees:tree(K, V) ) -> gb_trees:tree(K, V).

delete_if(Pred, Tree)
        when is_function(Pred, 2)  ->
    filter(fun(K, V) -> not Pred(K, V) end, Tree).
    