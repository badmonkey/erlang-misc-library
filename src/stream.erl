
-module(stream).

-export([ stddev_new/0, stddev_push/2, stddev_values/1
        , stddev_variance/1, stddev_variance/2, stddev_samples/1
        , stddev_test/0]).
-export([ sample_new/1, sample_push/2, sample_values/1
        , sample_test/0]).
-export([ minmax_new/0, minmax_push/2, minmax_values/1]).
        

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
           , #stream_minmax{ min = Min
                           , max = Max })
        when X < Min  ->
    #stream_minmax{ min = X, max = Max };
        
        
minmax_push( X
           , #stream_minmax{ min = Min
                           , max = Max })
        when X > Max  ->
    #stream_minmax{ min = Min, max = X };
        
        
minmax_push( _X
           , #stream_minmax{} = State) ->
    State.
        
         
minmax_values( #stream_minmax{ min = Min, max = Max } ) ->                       
    {Min, Max}.
    
    
%%%%% ------------------------------------------------------- %%%%%


-record(stream_frequent,
    { k			= 1			:: type:natural()
	, data		= #{}		:: #{ term() => type:natural() }
    }).


    
-spec frequent_new( type:natural() ) -> #stream_frequent{}.

frequent_new(K)
		when K > 1  ->
    #stream_frequent{ k = K }. 
    
    
frequent_push( X
			 , #stream_frequent{ k = K
							   , data = Data } = State ) ->
	Sz = maps:size(Data),
	case maps:get(X, Data, undefined) of
		undefined when Sz < K - 1	->
			#stream_frequent{ k = K, data = maps:put(X, 1, Data) }
			
	;	undefined					->
			NewData = xmaps:mutate(
							fun (K, 1) -> remove
							;   (K, V) -> V - 1
							end, Data),
			#stream_frequent{ k = K, data = NewData }
	
	;	Val							->
			#stream_frequent{ k = K, data = maps:put(X, Val + 1, Data) }
	end.



% Here's a simple description of Misra-Gries' Frequent algorithm. Demaine (2002) and
% others have optimized the algorithm, but this gives you the gist.

% Specify the threshold fraction, 1 / k; any item that occurs more than n / k times will
% be found. Create an an empty map (like a red-black tree); the keys will be search terms,
% and the values will be a counter for that term.

% Look at each item in the stream.
% If the term exists in the map, increment the associated counter.
% Otherwise, if the map less than k - 1 entries, add the term to the map with a counter of one.
% However, if the map has k - 1 entries already, decrement the counter in every entry.
% If any counter reaches zero during this process, remove it from the map.
% Note that you can process an infinite amount of data with a fixed amount of storage
% (just the fixed-size map). The amount of storage required depends only on the threshold of
% interest, and the size of the stream does not matter.

