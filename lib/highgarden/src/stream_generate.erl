
-module(stream_generate).

-export([take/2]).
-export([ thuemorse_new/0 ]).


%%%%% ------------------------------------------------------- %%%%%


-type result(T) :: { {value, _}, T } | { end_sequence, T } | type:error().
-type generator(T) :: { generator
                      , fun( (T) -> result(T) )
                      , T
                      }.

                      
take(0, {generator,_,_} = Generator) ->
    {[], Generator};
    
take(N, {generator,_,_} = Generator) ->    
    take_impl(N, Generator, []).
    

take_impl(0, GenOrErr, Accum) ->
    { lists:reverse(Accum), GenOrErr };
    
take_impl(N, {generator, Pop, State}, Accum) ->
    case Pop(State) of
        { {value, X}, State2 }      -> take_impl(N - 1, {generator, Pop, State2}, [X | Accum])
    ;   { end_sequence, State2 }    -> take_impl(0, {generator, Pop, State2}, Accum)
    ;   { error, _ } = Err          -> take_impl(0, Err, Accum)
    end.

    
    
%%%%% ------------------------------------------------------- %%%%%


-record(thuemorse,
    { data      = thuemorse_0() :: binary()
    , bitqueue                  :: queue:queue(boolean())
    }).
    
    
%% 64 values
thuemorse_0() ->    % 0-1-1-0
    <<2#0110100110010110, 2#1001011001101001, 2#1001011001101001, 2#0110100110010110
    , 2#1001011001101001, 2#0110100110010110, 2#0110100110010110, 2#1001011001101001
    , 2#1001011001101001, 2#0110100110010110, 2#0110100110010110, 2#1001011001101001
    , 2#0110100110010110, 2#1001011001101001, 2#1001011001101001, 2#0110100110010110>>.
    
thuemorse_1() ->    % 1-0-0-1
    <<2#1001011001101001, 2#0110100110010110, 2#0110100110010110, 2#1001011001101001
    , 2#0110100110010110, 2#1001011001101001, 2#1001011001101001, 2#0110100110010110
    , 2#0110100110010110, 2#1001011001101001, 2#1001011001101001, 2#0110100110010110
    , 2#1001011001101001, 2#0110100110010110, 2#0110100110010110, 2#1001011001101001>>.

    
    
-spec thuemorse_new() -> generator(#thuemorse{}).
    
thuemorse_new() ->
    { generator
    , fun thuemorse_pop/1
    , #thuemorse{ data = thuemorse_0(), bitqueue = queue:in(true, queue:new()) }
    }.
    

thuemorse_pop(#thuemorse{ bitqueue = Q, data = <<>> }) ->
    case queue:out(Q) of
        { {value, Next}, Q2 }   ->
            Q3 = queue:in(Next, Q2),
            Q4 = queue:in(not Next, Q3),
            
            Data2 = case Next of
                        false   -> thuemorse_0()
                    ;   true    -> thuemorse_1()
                    end,
                    
            thuemorse_pop( #thuemorse{
                                  bitqueue = Q4
                                , data = Data2
                                } )
                
    ;   _                       -> {error, morequeue_is_empty}
    end;
    
thuemorse_pop( #thuemorse{ data = <<X:1, Rest/bitstring>> } = State ) ->
    { { value, X =:= 1 }
    , State#thuemorse{ data = Rest }
    }.


%%%%% ------------------------------------------------------- %%%%%
    
