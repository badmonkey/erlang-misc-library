
-module(test_record).
-compile([{parse_transform, record_info_runtime}]).

-export([test/0]).
-export([records/0, record_info_fieldtypes/1]).
-export([dump_record/0, dump_record2/0]).
-export([test_tuple/0, print_result/1, print_result/2, print_result/3, print_result/4, andthen/2]).


test() ->
    X = [1, 2, 3, 4, 5],
    xlists:max(X).
    

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
    
    
%
%Closure = continue:eval(fun() -> end)
%                  :and_then(fun(X) -> end)
%                  :and_then(fun(X) -> end),
%

print_result(A) -> erlang:display({result, 1, A}).
print_result(A, B) -> erlang:display({result, 2, A, B}).
print_result(A, B, C) -> erlang:display({result, 3, A, B, C}).
print_result(A, B, C, D) -> erlang:display({result, 3, A, B, C, D}).

andthen(New, {xshell, Current}) ->
    {xshell, {New, Current}}.

test_tuple() ->
%    {xshell,42}:print_result(),
%    {xshell,42}:print_result("hello"),
%    {xshell,{data,42}}:print_result("hello"),
%    C = {xshell,{first, 1}}:andthen({second,2}),
%    C:andthen({third,3}),
    ( {xshell,{first, 1}}:andthen({second,2}) ):andthen({third,3}),
    ok.
    