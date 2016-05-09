
-module(jsonx).

-export([ decode/1, get/2, get/3
        , get_as/3, get_as/4, get_or_throw/2
        , get_any/2, get_any/3, get_all/2, get_all/3]).
-export([ as_boolean/1, as_integer/1, as_bstring/1]).


%%%%% ------------------------------------------------------- %%%%%


-type js_string()       :: binary().
-type js_null()         :: null.
-type js_number()       :: integer() | float().
-type js_boolean()      :: boolean().
-type js_array()        :: [js_term()].
-type js_plist()        :: [{js_string(), js_term()}].
-type js_object()       :: { js_plist() } | #{ js_string() => js_term() }.
-type js_value()        :: js_string() | js_number() | js_null() | js_boolean().
-type js_term()         :: js_value() | js_array() | js_object() | undefined.


-type array_select()    :: { select
                           , all | {js_string(), js_value()}
                           }.

-type key()             :: binary() | integer() | first | last
                         | array_select() | string() | atom().

-type key_path()        :: [ key() ].


-export_type([ js_term/0, js_value/0, js_object/0, js_plist/0, js_array/0
             , js_boolean/0, js_number/0, js_null/0, js_string/0]).
-export_type([ key_path/0, key/0, array_select/0]).


%%%%% ------------------------------------------------------- %%%%%


-spec decode( binary() ) -> js_object().

decode(Json) when is_binary(Json) ->
    jiffy:decode(Json, [return_maps]).


%%%%% ------------------------------------------------------- %%%%%


-type get_key()     :: binary() | key_path().
-type get_json()    :: binary() | js_object().
-type get_default() :: jsthrow | js_term().


get(Key, Json) ->
    get(Key, Json, undefined).
    

-spec get( get_key(), get_json(), get_default() ) -> js_term() | type:exception().
    
get(Key, Json, Default)
        when is_binary(Json)  ->
    get(Key, decode(Json), Default);

get(Key, Json, Default)
        when is_binary(Key)  ->
    get( jsonx_search:parse_path(Key), Json, Default);
    
        
get(Key, Json, Default) ->        
    jsonx_search:search(Key, Json, Default).
    
    
%%%%% ------------------------------------------------------- %%%%%


get_as(Type, Key, Json) ->
    get_as(Type, Key, Json, jsthrow).
    
    
-spec get_as(integer, get_key(), get_json(), js_throw | integer()) -> integer() | type:exception();
            (boolean, get_key(), get_json(), js_throw | boolean()) -> boolean() | type:exception().
              
get_as(integer, Key, Json, Default) ->
    as_integer( get(Key, Json, Default) );
    
get_as(boolean, Key, Json, Default) ->
    as_boolean( get(Key, Json, Default) ).
    

%%%%% ------------------------------------------------------- %%%%%
    

get_or_throw(Key, Json) ->
    get(Key, Json, jsthrow).


%%%%% ------------------------------------------------------- %%%%%


get_any(Keys, Json) ->
    get_any(Keys, Json, undefined).
    

get_any(Keys, Json, Default)
        when is_binary(Json)  ->
    get_any(Keys, decode(Json), Default);

    
get_any([], _Json, jsthrow) ->
    throw({error, no_matches});
    
get_any([], _Json, Default) ->
    Default;

get_any([Head | Rest], Json, Default) ->
    case get(Head, Json, undefined) of
        undefined   -> get_any(Rest, Json, Default)
    ;   X           -> X
    end.

    
%%%%% ------------------------------------------------------- %%%%%


get_all(Keys, Json) ->
    get_all(Keys, Json, undefined).
    
    
get_all(Keys, Json, Default)
        when is_binary(Json)  ->
    get_all(Keys, decode(Json), Default);
    
get_all(Keys, Json, Default) ->
    [ get(X, Json, Default) || X <- Keys ].
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec as_boolean( binary() | string() | integer() ) -> boolean() | type:exception().

as_boolean(<<"true">>)          -> true;
as_boolean(<<"false">>)         -> false;
as_boolean(B) when is_binary(B) ->
    as_boolean( xerlang:binary_to_integer(B) );

as_boolean("true")              -> true;
as_boolean("false")             -> false;
as_boolean("yes")               -> true;
as_boolean("no")                -> false;
as_boolean(L) when is_list(L)   ->
    as_boolean( list_to_integer(L) );
    
as_boolean(0)                   -> false;
as_boolean(1)                   -> true;
as_boolean(X) when is_integer(X)-> X =/= 0;

as_boolean(X) -> throw({error, {not_a_bool, X}}).


%%%%% ------------------------------------------------------- %%%%%


-spec as_integer( binary() | string() | integer() ) -> integer().

as_integer(B) when is_binary(B) ->
    as_integer( xerlang:binary_to_integer(B) );

as_integer(L) when is_list(L) ->
    as_integer( list_to_integer(L) );
    
as_integer(X) when is_integer(X) -> X;

as_integer(X) -> throw({error, {not_a_int, X}}).


%%%%% ------------------------------------------------------- %%%%%


-spec as_bstring( binary() | string() | integer() ) -> binary().

as_bstring(X) ->
    X.

%as_string()
