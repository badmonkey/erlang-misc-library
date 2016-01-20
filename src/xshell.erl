
-module(xshell).
-compile([{parse_transform, record_info_runtime}]).

-export([records/0, record_info_fieldtypes/1]).
-export([dump_record/0, dump_record2/0]).

-record(coord,
    { x
    , y
    }).
    
-record(test0,
    { name = "fred" :: string()
    , location :: #coord{}
    , health = 1
    , mana :: undefined
    }).
    

dump_record() ->
    erlang:display( record_info(fields, test0) ).
    
dump_record2() ->
    erlang:display( record_info_fieldtypes(test0) ).    