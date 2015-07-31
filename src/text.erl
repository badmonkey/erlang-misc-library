
-module(text).

-include_lib("erlangx/include/constants.hrl").


-export([is_format/1, pluralize/2, pluralize/3, pluralize/4]).
-export([short_duration/1, long_duration/3, timesince/1, timesince/2]).


-define(DEFAULT_PRECISION, 2).



%%%%% ------------------------------------------------------- %%%%%


-spec is_format( string() ) -> boolean().

is_format(X) ->
    string:chr(X, $~) =/= 0.


guess_formatting(Number, String) ->
    case is_format(String) of
        true                            -> String
    ;   false when is_integer(Number)   -> "~B" ++ String
    ;   false when is_float(Number)     -> "~f" ++ String
    ;   _Else                           -> String
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec pluralize( number(), type:format() ) -> string().

pluralize(Number, Singular) ->
    Format = guess_formatting(Number, Singular),
    pluralize(Number, Format, Format ++ "s", ?DEFAULT_PRECISION).


-spec pluralize( number(), type:format(), type:format() | non_neg_integer() ) -> string().    
    
pluralize(Number, Singular, Precision)
        when is_integer(Precision)  ->
    Format = guess_formatting(Number, Singular),
    pluralize(Number, Format, Format ++ "s", Precision);
    
pluralize(Number, Singular, Plural) ->
    pluralize( Number
             , guess_formatting(Number, Singular)
             , guess_formatting(Number, Plural)
             , ?DEFAULT_PRECISION).


-spec pluralize( number(), type:format(), type:format(), non_neg_integer() ) -> string().
    
pluralize(Number, Singular, Plural, Precision) ->
    Value = xmaths:round(Number, Precision),
    Out =   case Value of
                1   -> io_lib:format(Singular, [Value])
            ;   _   -> io_lib:format(Plural, [Value])
            end,
    lists:flatten(Out).
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec short_duration( non_neg_integer() ) -> string().

short_duration(Time) ->
    if
		Time >= ?SECS_PER_DAY       -> pluralize(Time / ?SECS_PER_DAY, " day")
	;	Time >= ?SECS_PER_HOUR      -> pluralize(Time / ?SECS_PER_HOUR, " hr")
    ;   Time >= ?SECS_PER_MIN       -> pluralize(Time / ?SECS_PER_MIN, " min")
    ;   Time >= 1 orelse Time =:= 0 -> pluralize(Time, " sec")

    ;   Time >= ?MILLISEC_PER_SEC   -> pluralize(Time / ?MILLISEC_PER_SEC, " mSec")
    ;   Time >= ?MICROSEC_PER_SEC   -> pluralize(Time / ?MICROSEC_PER_SEC, " uSec")
    ;   true                        -> pluralize(Time / ?NANOSEC_PER_SEC, " nSec")
    end.

    
%%%%% ------------------------------------------------------- %%%%%
    
    
defer_pluralize(Singular, Plural) ->
    fun(Number) ->
        pluralize(Number, Singular, Plural, 0)
    end.
    
    
time_ranges() ->
    [ {?SECS_PER_WEEK,  defer_pluralize("~B week",   "~B weeks")}
    , {?SECS_PER_DAY,   defer_pluralize("~B day",    "~B days")}
    , {?SECS_PER_HOUR,  defer_pluralize("~B hour",   "~B hours")}
    , {?SECS_PER_MIN,   defer_pluralize("~B minute", "~B minutes")}
    ].
    
    
    
-spec long_duration( non_neg_integer(), non_neg_integer(), boolean() ) -> string().    
    
long_duration(Time, MaxParts, ShowRemain) ->
    {Remain, _, Parts} =
            lists:foldl(
                    fun ( _, {_, Cnt, _} = Acc ) when Cnt =:= MaxParts  ->
                            Acc
                            
                    ;   ( {Val, F}, {DeltaT, Cnt, Parts} = Acc ) ->
                            Amt = DeltaT div Val,
                            case Amt of
                                0 -> Acc
                                
                            ;   _ ->
                                    { DeltaT - (Val * Amt)
                                    , Cnt + 1
                                    , [F(Amt) | Parts] }
                            end
                    end
                , {Time, 0, []}
                , time_ranges() ),

    FullParts = case ShowRemain of
                    true    -> [ short_duration(Remain) | Parts ]
                ;   false   -> Parts
                end,
    string:join(lists:reverse(FullParts), " ").
    
    
    
-define(TIMESINCE_MAXPARTS, 2).


-spec timesince( non_neg_integer() ) -> string().
-spec timesince( non_neg_integer(), non_neg_integer() ) -> string().

timesince(FromTime) -> long_duration(xtime:in_seconds() - FromTime, ?TIMESINCE_MAXPARTS, false).
timesince(FromTime, ToTime) -> long_duration(ToTime - FromTime, ?TIMESINCE_MAXPARTS, false).


%%%%% ------------------------------------------------------- %%%%%
