
-module(xets).
-extends(ets).

-export([match_delete/2]).



%%%%% ------------------------------------------------------- %%%%%


-spec match_delete( ets:tab(), ets:match_pattern() ) -> true.

match_delete(Table, Spec) ->
    [ ets:delete_object(Table, X) || X <- ets:match_object(Table, Spec) ],
    true.

    