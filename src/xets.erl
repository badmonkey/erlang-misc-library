
-module(xets).

-export([match_delete/2, foreach/2, foreachx/2]).



%%%%% ------------------------------------------------------- %%%%%


-spec match_delete( ets:tab(), ets:match_pattern() ) -> true.

match_delete(Table, Spec) ->
    [ ets:delete_object(Table, X) || X <- ets:match_object(Table, Spec) ],
    true.


%%%%% ------------------------------------------------------- %%%%%

    
-spec foreach( fun((_) -> _), ets:tab() ) -> ok.

foreach(Fun, Table) ->
    ets:foldl(
          fun(X, undefined) ->
            Fun(X),
            undefined
          end
        , undefined
        , Table),
    ok.
        

%%%%% ------------------------------------------------------- %%%%%

    
-spec foreachx( fun((_) -> type:ok_or_error()), ets:tab() ) -> type:ok_or_error().

foreachx(Fun, Table) ->
    ets:foldl(
          fun(X, ok)            -> Fun(X)
          ;  (_, {error, E})    -> {error, E}
          end
        , ok
        , Table).

