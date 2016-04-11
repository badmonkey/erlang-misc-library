
-module(xsets).

-export([take/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec take( pos_integer(), sets:set(T) ) -> [T].

take(N, S) ->
    lists:sublist(sets:to_list(S), N).

