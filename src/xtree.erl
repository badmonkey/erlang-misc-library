
-module(xtree).

-export([delete_all/2, foreach/2, filter/2, delete_if/2]).



%%%%% ------------------------------------------------------- %%%%%


-spec delete_all( [K], gb_tree:tree(K, V) ) -> gb_tree:tree(K, V).

delete_all(Keys, Tree) ->
    lists:foldl(
            fun(X, T) ->
                gb_tree:delete_any(X, T)
            end
        , Tree, Keys).


%%%%% ------------------------------------------------------- %%%%%


-spec foreach( fun((K, V) -> term()), gb_tree:tree(K, V) ) -> gb_tree:tree(K, V).

foreach(Pred, Tree) ->
    Tree.


%%%%% ------------------------------------------------------- %%%%%


-spec filter( fun((K, V) -> boolean()), gb_tree:tree(K, V) ) -> gb_tree:tree(K, V).

filter(Pred, Tree)
        when is_function(Pred, 2)  ->
        
    NewTree = lists:filter(
                    fun({Key, Value}) ->
                        Pred(Key, Value)
                    end
                , gb_tree:to_list(Tree) ),
    gb_tree:from_orddict(NewTree).
    

%%%%% ------------------------------------------------------- %%%%%


-spec delete_if( fun((K, V) -> boolean()), gb_tree:tree(K, V) ) -> gb_tree:tree(K, V).

delete_if(Pred, Tree)
        when is_function(Pred, 2)  ->
    filter(fun(K, V) -> not Pred(K, V) end, Tree).
    