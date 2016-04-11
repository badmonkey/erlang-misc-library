
-module(stream).

-export([ stddev_new/0, stddev_push/2, stddev_values/1
        , stddev_variance/1, stddev_variance/2, stddev_samples/1
        , stddev_test/0]).
-export([ sample_new/1, sample_push/2, sample_values/1
        , sample_test/0]).
-export([ minmax_new/0, minmax_new/1, minmax_push/2
        , minmax_values/1, minmax_median/1, minmax_samples/1]).
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
    
    
% Based on Welford's method for calculating standard deviations.
    
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
    { n         = 0         :: type:natural()
    , min       = undefined :: number() | undefined
    , max       = undefined :: number() | undefined
    , estmedian = undefined :: number() | undefined
    }).

    
    
-spec minmax_new( number() ) -> #stream_minmax{}.

minmax_new() -> minmax_new(0).

minmax_new(Est) ->
    #stream_minmax{ estmedian = Est }.    
    
    
minmax_push( X
           , #stream_minmax{ n = 0
                           , estmedian = undefined }) ->
    #stream_minmax{ n = 1, min = X, max = X, estmedian = X };
    
    
minmax_push( X
           , #stream_minmax{ n = 0
                           , estmedian = Est }) ->
    NwEst =     if
                    X > Est -> Est + 1
                ;   X < Est -> Est - 1
                ;   true    -> Est
                end,
    #stream_minmax{ n = 1, min = X, max = X, estmedian = NwEst };
    
                           
minmax_push( X
           , #stream_minmax{ n = N
                           , min = Min, max = Max
                           , estmedian = Est }) ->
    {NwMin, NwMax} =    if
                            X < Min     -> {X,   Max}
                        ;   X > Max     -> {Min, X}
                        ;   true        -> {Min, Max}
                        end,
    NwEst =     if
                    X > Est -> Est + 1
                ;   X < Est -> Est - 1
                ;   true    -> Est
                end,
    #stream_minmax{ n = N + 1, min = NwMin, max = NwMax, estmedian = NwEst }.

         
minmax_values( #stream_minmax{ min = Min, max = Max } ) -> {Min, Max}.
minmax_median( #stream_minmax{ estmedian = Est } ) -> Est.        
minmax_samples( #stream_minmax{ n = N } ) -> N.    
    
    
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
    

    
    
%def frugal_2u(stream, m = 0, q = 0.5, f = constantly_one):
%  step, sign = 1, 1
 
%for item in stream:
%  if item > m and random() > 1 - q:
%    # Increment the step size if and only if the estimate keeps moving in
%    # the same direction. Step size is incremented by the result of applying
%    # the specified step function to the previous step size.
%    step += f(step) if sign > 0 else -1 * f(step)
%    # Increment the estimate by step size if step is positive. Otherwise,
%    # increment the step size by one.
%    m += step if step > 0 else 1
%    # Mark that the estimate increased this step
%    sign = 1
%    # If the estimate overshot the item in the stream, pull the estimate back
%    # and re-adjust the step size.
%    if m > item:
%      step += (item - m)
%      m = item
%  # If the item is less than the stream, follow all of the same steps as
%  # above, with signs reversed.
%  elif item < m and random() > q:
%    step += f(step) if sign < 0 else -1 * f(step)
%    m -= step if step > 0 else 1
%    sign = -1
%    if m < item:
%      step += (m - item)
%      m = item
%  # Damp down the step size to avoid oscillation.
%  if (m - item) * sign < 0 and step > 1:
%    step = 1 