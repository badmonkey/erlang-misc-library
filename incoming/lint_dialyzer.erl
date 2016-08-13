
-module(lint_dialyzer).

-export([test1/1, test2/1, test3/1, test4/0, test5/0]).


%%%%% ------------------------------------------------------- %%%%%


-record(record1,
    { id            :: atom()
    , count         :: number()
    }).


%%%%% ------------------------------------------------------- %%%%%


-spec value_or_undefined( number() ) -> undefined | number().

value_or_undefined(X) ->
    case X of
        0   -> undefined
    ;   _   -> X + 1
    end.
    
    
-spec enum_or_undefined( number() ) -> undefined | val1 | val2.    

enum_or_undefined(X) ->
    case X of
        0   -> undefined
    ;   1   -> val1
    ;   _   -> val2
    end.
    

-spec record_or_undefined( number() ) -> undefined | #record1{}.

record_or_undefined(X) ->
    case X of
        42  -> undefined
    ;   _   -> #record1{ id = a_test, count = X + 1 }
    end.


%%%%% ------------------------------------------------------- %%%%%
    
        
test1(X) ->
    case value_or_undefined(X) of
        1                       -> 1
    ;   Y when is_integer(Y)    -> 2
    end.
    
    
test2(X) ->
    #record1{ id = a_test
            , count = Num
            }  = record_or_undefined(X),
    Num.
    
    
test3(X) ->    
    case enum_or_undefined(X) of
        val1    -> true
    ;   val2    -> false
    end.


test4() ->
    test4a( record_or_undefined(3) ).
    
test4a(undefined) -> ok.    


test5() ->
    #record1{ id = a_test
            , count = _Num
            }  = enum_or_undefined(7),
    ok.

