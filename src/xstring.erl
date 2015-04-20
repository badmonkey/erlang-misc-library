
-module(xstring).

-export([hex_to_int/1, format/2, token_map/3]).


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


-spec token_map( fun((string()) -> string()), string(), string() ) -> string().

token_map(F, S, Sep)
        when  is_function(F, 1)
            , is_list(S)
            , is_list(Sep)  ->
    string:join( lists:map( F, string:tokens(S, Sep) ), Sep ).
    
    
%%%%% ------------------------------------------------------- %%%%%




