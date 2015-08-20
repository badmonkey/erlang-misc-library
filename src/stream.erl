
-module(stream).

-export([stddev_new/0, stddev_push/2, stddev_variance/1, stddev_values/1, stddev_samples/1]).
-export([sample_new/1, sample_update/2, sample_values/1]).


%%%%% ------------------------------------------------------- %%%%%


-record(stream_stddev,
    { ddof      = 1         :: pos_integer()
    , n         = 0         :: type:natural()
    , mean      = 0.0       :: float()
    , m2        = 0.0       :: float()
    , delta     = 0.0       :: float()
    }).
    
    
-record(stream_sample,
    { size      = 1         :: pos_integer()
    , count     = 0         :: type:natural()
    , data      = {}        :: tuple()
    }).


%%%%% ------------------------------------------------------- %%%%%


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

        
stddev_variance( #stream_stddev{ n = N
                               , ddof = Ddof } )
        when N =< Ddof ->
    0.0;
stddev_variance( #stream_stddev{ n = N
                               , m2 = M2
                               , ddof = Ddof } )    ->
    M2 / (N - Ddof).


stddev_values( #stream_stddev{ n = 0 } )            -> { 0.0, 0.0 };
stddev_values( #stream_stddev{ mean = Mean } = X )  ->
    { Mean, math:sqrt( stddev_variance(X) ) }.

    
stddev_samples( #stream_stddev{ n = N } ) -> N.


%%%%% ------------------------------------------------------- %%%%%


-spec sample_new( pos_integer() ) -> #stream_sample{}.

sample_new(N) ->
    #stream_sample{ size = N }.
    

sample_update( X
             , #stream_sample{ size = N
                             , count = Cnt
                             , data = Data } = State)
        when Cnt < N  ->
    State#stream_sample{ count = Cnt + 1
                       , data = erlang:append_element(Data, X) };
                       
                       
sample_update( X
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

                       
%%%%% ------------------------------------------------------- %%%%%

