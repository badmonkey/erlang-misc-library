
-module(xtime).

-export([in_seconds/0, in_milliseconds/0, now_in_milliseconds/0, unix_epoch/0, gregorian_epoch/0]).


%%%%% ------------------------------------------------------- %%%%%


% Because we're truncating the microsecs value there's no point using erlang:now to guarantee uniqueness.
% So subsequent calls to in_milliseconds are not guaranteed to be unique.
-spec in_milliseconds() -> pos_integer().

in_milliseconds() ->
    {MegaSec, Sec, MicroSec} = os:timestamp(),
    1000000000 * MegaSec + 1000 * Sec + erlang:trunc(MicroSec/1000).
    
    
in_seconds() ->
    {MegaSec, Sec, _} = os:timestamp(),
    1000000 * MegaSec + Sec.    
    

%%%%% ------------------------------------------------------- %%%%%


-spec now_in_milliseconds() -> float().

now_in_milliseconds() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    1000000000 * MegaSec + 1000 * Sec + MicroSec/1000.
    

%%%%% ------------------------------------------------------- %%%%%


-spec unix_epoch() -> non_neg_integer().

unix_epoch() ->    
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000000000 + Seconds * 1000000 + MicroSeconds).
    

%%%%% ------------------------------------------------------- %%%%%


% Offset between 15 October 1582 and 1 January 1970
-define(INTERVAL_OFFSET, 122192928000000000).
-define(INTERVAL_FACTOR, 10).


% The number of 100 nanosecond intervals since 15 October 1582
-spec gregorian_epoch() -> <<_:60>>.

gregorian_epoch() ->
    Timestamp = ?INTERVAL_OFFSET + ?INTERVAL_FACTOR * unix_epoch(),
    <<Timestamp:60>>.    
    
    