
-module(xstring).

-export([hex_to_int/1, format/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec hex_to_int( string() ) -> integer().

hex_to_int(Hex) ->
    {ok, [D], []} = io_lib:fread("~16u", Hex),
    D.
    

%%%%% ------------------------------------------------------- %%%%%

    
-spec format( io:format(), [term()] ) -> string().

format(S, P) ->
    lists:flatten(io_lib:format(S, P)).
    

%%%%% ------------------------------------------------------- %%%%%

