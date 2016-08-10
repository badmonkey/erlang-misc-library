
-module(xsets).

-export([take/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec take( type:cardinal(), sets:set(T) ) -> [T].

take(0, _) -> [];
take(N, S) ->
    lists:sublist(sets:to_list(S), N).

