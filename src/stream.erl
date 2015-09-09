
-module(stream).

-export([ stddev_new/0, stddev_push/2, stddev_values/1
        , stddev_variance/1, stddev_variance/2, stddev_samples/1
        , stddev_test/0]).
-export([ sample_new/1, sample_push/2, sample_values/1
        , sample_test/0]).


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

