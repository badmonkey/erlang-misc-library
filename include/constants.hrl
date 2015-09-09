

%%%%% ------------------------------------------------------- %%%%%
% Time based constants

-define(SECS_PER_MIN,           60).
-define(MINS_PER_HOUR,          60).
-define(HOURS_PER_DAY,          24).

-define(DAYS_PER_WEEK,          7).

-define(SECS_PER_HOUR,          ?SECS_PER_MIN * ?MINS_PER_HOUR).
-define(SECS_PER_DAY,           ?SECS_PER_HOUR * ?HOURS_PER_DAY).

-define(SECS_PER_WEEK,          ?SECS_PER_DAY * ?DAYS_PER_WEEK).

-define(MILLISEC_PER_SEC,       1000).
-define(MICROSEC_PER_MILLISEC,  1000).
-define(MICROSEC_PER_SEC,       ?MICROSEC_PER_MILLISEC * ?MILLISEC_PER_SEC).
-define(NANOSEC_PER_MICROSEC,   1000).
-define(NANOSEC_PER_SEC,        ?NANOSEC_PER_MICROSEC * ?MICROSEC_PER_SEC).

-define(MILLISEC_PER_MIN,       ?SECS_PER_MIN * ?MILLISEC_PER_SEC).
-define(MILLISEC_PER_HOUR,      ?MINS_PER_HOUR * ?MILLISEC_PER_MIN).


%%%%% ------------------------------------------------------- %%%%%
% Byte size constants

%constexpr value_type KILOBYTES              = 1024;
%constexpr value_type MEGABYTES              = 1024 * KILOBYTES;
%constexpr value_type GIGABYTES              = 1024 * MEGABYTES;
%constexpr value_type TERABYTES              = 1024 * GIGABYTES;

%constexpr value_type BITS_PER_BYTE          = 8;

