
-module(stream).

-export([ stddev_new/0, stddev_push/2, stddev_values/1
        , stddev_variance/1, stddev_variance/2, stddev_samples/1
        , stddev_test/0]).
-export([ sample_new/1, sample_push/2, sample_values/1
        , sample_test/0]).
-export([ minmax_new/0, minmax_push/2, minmax_values/1]).
-export([ fold_new/2, fold_push/2, fold_values/1]).
-export([ frequent_new/1, frequent_push/2, frequent_values/1]).
        

%%%%% ------------------------------------------------------- %%%%%


-record(stream_stddev,
    { n         = 0         :: type:natural()
    , mean      = 0.0       :: float()
    , m2        = 0.0       :: float()
    , delta     = 0.0       :: float()
    }).



stddev_new() ->
    #stream_stddev{}.
    
    
% Based on Welford's method for calculating stardard deviations.
    
-spec stddev_push( number(), #stream_stddev{} ) -> #stream_stddev{}.
    
stddev_push( X
           , #stream_stddev{ n = N
                           , mean = Mean
                           , m2 = M2 } = State ) ->

    NwN = N + 1,
    Delta = X - Mean,
    NwMean = Mean + Delta / NwN,
    NwM2 = M2 + Delta * (X - NwMean),
                           
    State#stream_stddev{
              n         = NwN
            , mean      = NwMean
            , m2        = NwM2
            , delta     = Delta
        }.

        
stddev_variance( #stream_stddev{} = State )         -> stddev_variance(0.0, State).


-spec stddev_variance( float(), #stream_stddev{} ) -> float() | undefined.
        
stddev_variance( Ddof
               , #stream_stddev{ n = N } )
        when N =< Ddof ->
    undefined;

stddev_variance( Ddof
               , #stream_stddev{ n = N
                               , m2 = M2 } )        ->
    M2 / (N - Ddof).


stddev_values( #stream_stddev{ n = 0 } )            -> { 0.0, 0.0 };
stddev_values( #stream_stddev{ mean = Mean } = X )  ->
    { Mean, math:sqrt( stddev_variance(X) ) }.

    
stddev_samples( #stream_stddev{ n = N } ) -> N.


stddev_test() ->
    Values = [2,4,4,4,5,5,7,9],
    Out = lists:foldl(
                 fun(X, #stream_stddev{} = State) ->
                    stddev_push(X, State)
                 end
               , stddev_new()
               , Values ),
    stddev_values(Out) =:= xmaths:std_deviation2(Values).


%%%%% ------------------------------------------------------- %%%%%


-record(stream_sample,
    { size      = 1         :: pos_integer()
    , count     = 0         :: type:natural()
    , data      = {}        :: tuple()
    }).
    
    

-spec sample_new( pos_integer() ) -> #stream_sample{}.

sample_new(N) ->
    #stream_sample{ size = N }.
    

sample_push( X
           , #stream_sample{ size = N
                           , count = Cnt
                           , data = Data } = State)
        when Cnt < N  ->
    State#stream_sample{ count = Cnt + 1
                       , data = erlang:append_element(Data, X) };
                       
                       
sample_push( X
           , #stream_sample{ size = N
                           , count = Cnt
                           , data = Data } = State) ->
    R = rand:uniform(Cnt + 1),
    NewData =   case R =< N of
                    true    -> erlang:setelement(R, Data, X)
                ;   false   -> Data
                end,
    State#stream_sample{ count = Cnt + 1
                       , data = NewData }.

                       
sample_values( #stream_sample{ data = Data } ) ->                       
    erlang:tuple_to_list(Data).
    
    
sample_test() ->
    Out = lists:foldl(
                  fun(X, Acc) ->
                    sample_push(X, Acc)
                  end
                , sample_new(3)
                , [1,a,2,b,3,c,4,d,5,e,6,f,7,g] ),
    sample_values(Out).

           
%%%%% ------------------------------------------------------- %%%%%


-record(stream_minmax,
    { min       = undefined :: number() | undefined
    , max       = undefined :: number() | undefined
    }).

    
    
-spec minmax_new() -> #stream_minmax{}.

minmax_new() ->
    #stream_minmax{}.    
    
    
minmax_push( X
           , #stream_minmax{ min = undefined
                           , max = undefined }) ->
    #stream_minmax{ min = X, max = X };
    
                           
minmax_push( X
           , #stream_minmax{ min = Min, max = Max })
        when X < Min  ->
    #stream_minmax{ min = X, max = Max };
        
        
minmax_push( X
           , #stream_minmax{ min = Min, max = Max })
        when X > Max  ->
    #stream_minmax{ min = Min, max = X };
        
        
minmax_push( _X
           , #stream_minmax{} = State) ->
    State.
        
         
minmax_values( #stream_minmax{ min = Min, max = Max } ) ->                       
    {Min, Max}.
    

%%%%% ------------------------------------------------------- %%%%%


-record(stream_fold,
    { func                  :: fun()
    , accum                 :: term()
    }).

    
    
-spec fold_new( fun((term(), A) -> A), A ) -> #stream_fold{}.

fold_new(F, A) ->
    #stream_fold{ func = F, accum = A }.    
    
    
fold_push( X
         , #stream_fold{ func = F, accum = A }) ->
    #stream_fold{ func = F, accum = F(X, A) }.
            
         
fold_values( #stream_fold{ accum = A } ) -> A.
    
        
%%%%% ------------------------------------------------------- %%%%%


-record(stream_frequent,
    { k         = 1         :: type:natural()
    , n         = 0         :: type:natural()
    , data      = #{}       :: #{ term() => pos_integer() }
    }).


    
% Based on Misra-Gries' Frequent algorithm.
    
-spec frequent_new( type:natural() ) -> #stream_frequent{}.

frequent_new(K) when K > 1  ->
    #stream_frequent{ k = K, n = 0 }. 
    
    
frequent_push( X
             , #stream_frequent{ k = K, n = N, data = Data }) ->
    Sz = maps:size(Data),
    case maps:get(X, Data, undefined) of
        undefined when Sz < K - 1   ->
            #stream_frequent{ k = K, n = N + 1, data = maps:put(X, 1, Data) }
            
    ;   undefined                   ->
            NewData = xmaps:mutate(
                            fun (_, 1) -> remove
                            ;   (_, V) -> V - 1
                            end, Data),
            #stream_frequent{ k = K, n = N + 1, data = NewData }
    
    ;   Val                         ->
            #stream_frequent{ k = K, n = N + 1, data = maps:put(X, Val + 1, Data) }
    end.

    
frequent_values( #stream_frequent{ n = N
                                 , data = Data }) ->    
    { N, maps:to_list(Data) }.
    
