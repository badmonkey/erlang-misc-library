
-module(mqtt_path).


-export([new_publish/1, new_subscribe/1, create_path/1]).


-type input_type() :: string() | binary().
-type input_path() :: [input_type() | root | match_one | match_star].


%%%%% ------------------------------------------------------- %%%%%


-spec new_publish( input_type() ) -> router:publish_path() | type:error().

new_publish(Input) ->
    new_path(Input, false).
    
    
-spec new_subscribe( input_type() ) -> router:subscribe_path() | type:error().

new_subscribe(Input) ->    
    new_path(Input, true).

    
%%%%% ------------------------------------------------------- %%%%%
    

new_path(Input, AllowWild) ->
    Path = create_path(Input),
    case check_path(Path, AllowWild) of
        ok  -> Path
    ;   Err -> Err
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%    


-spec create_path( input_type() ) -> input_path().
    
create_path("/" ++ Rest) ->
    [root | create_path(Rest)];
    
create_path(<<"/", Rest/binary>>) ->
    [root | create_path(Rest)];
    
create_path(Input) ->
    replace_wildcards( split(Input) ).
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

split(Input) when is_binary(Input)  ->
    binary:split(Input, <<"/">>, [global]);
    
split(Input) ->
    re:split(Input,"[/]",[{return,list}]).
    
    
%%%%% ------------------------------------------------------- %%%%%    

    
-spec replace_wildcards( [input_type()] ) -> input_path().

replace_wildcards([]) -> [];

replace_wildcards(["+" | Rest])
    -> [match_one | replace_wildcards(Rest)];
    
replace_wildcards([<<"+">> | Rest])
    -> [match_one | replace_wildcards(Rest)];    
    
replace_wildcards(["#" | Rest])
    -> [match_star | replace_wildcards(Rest)];
    
replace_wildcards([<<"#">> | Rest])
    -> [match_star | replace_wildcards(Rest)];    
    
replace_wildcards([Hd | Rest])
    -> [Hd | replace_wildcards(Rest)].
    
    
%%%%% ------------------------------------------------------- %%%%%   


-spec check_path( input_path(), boolean() ) -> type:ok_or_error().

check_path([], _)                           -> ok;
check_path([[]], _)                         -> {error, path_ends_with_a_seperator};
check_path([[] | _Rest], _)                 -> {error, path_contains_empty_part};
check_path([match_star], AllowWild)         -> valid_wildcard(AllowWild);
check_path([match_star | _Rest], _)         -> {error, wildcard_not_allowed_here};

check_path([match_one | Rest], AllowWild)   ->
    case valid_wildcard(AllowWild) of
        ok  -> check_path(Rest, AllowWild)
    ;   Err -> Err
    end;
    
check_path([Hd | Rest], AllowWild)          ->
    case is_valid_name(Hd) of
        true    -> check_path(Rest, AllowWild)
    ;   false   -> {error, invalid_chars_in_path}
    end.

    

valid_wildcard(true) -> ok;
valid_wildcard(false) -> {error, wildcards_are_invalid}.


is_valid_name(root)         -> true;
is_valid_name(match_one)    -> true;
is_valid_name(match_star)   -> true;

% TODO work out how to check for valid chars in binary
is_valid_name(B) when is_binary(B)  -> true;

is_valid_name(ID)           ->
    case re:run(ID, "^[a-zA-Z0-9_ ,%$!@<>()=:;.?|\-]+$") of 
        { match, _ }    -> true
    ;   _               -> false
    end.
    
    
