
-module(stream).

-export([stddev_new/0, stddev_push/2, stddev_variance/1, stddev_values/1]).


%%%%% ------------------------------------------------------- %%%%%


-record(stream_stddev,
    { ddof      = 1         :: pos_integer()
    , n         = 0         :: type:natural()
    , mean      = 0.0       :: float()
    , m2        = 0.0       :: float()
    , delta     = 0.0       :: float()
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

        
stddev_variance( #stream_stddev{ n = 1 } )          -> 0.0;
stddev_variance( #stream_stddev{ n = N
                               , m2 = M2
                               , ddof = Ddof } )    ->
    M2 / (N - Ddof).


stddev_values( #stream_stddev{ n = 0 } )            -> { 0.0, 0.0 };
stddev_values( #stream_stddev{ mean = Mean } = X )  ->
    { Mean, math:sqrt( stddev_variance(X) ) }.


%%%%% ------------------------------------------------------- %%%%%
