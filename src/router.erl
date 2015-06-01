
-module(router)


-type path_fragment() :: string() | binary()
            | match_star | {match_star, atom()}
            | match_one | {match_one, atom()} | {match_one, atom(), {term(), term()}}.
            
-type path() :: [path_fragment()].


-spec add( routerid :: term(), path(), data() ) -> ok_or_error().

-spec get( routerid :: term(), path(), Args :: term() ) -> [data() | {data(), propslist()}].


-record(router,
    { routerid  :: term()
    , nodeid    :: pos_integer()
    }).

-record(route_node,
    { nodeid    :: pos_integer()
    , name      :: path_fragment()
    , data      :: [data()]
    , children  :: [pos_integer()]
    }).
    


% ["a", "b"]
% ["a", "e"]
% ["a", "b", {match_star, some_name}]

% {[], "a", ["a"]}
% {["a"], "b", undefined}
% {["a"], "e", undefined}




% a, b, * -> X1, Y1
% a, b, c -> X2
% a, b, + -> X3
% a, b, c, d -> X4
% a, +, c -> X5

% [a, b] -> X1 * = []
% [a, b, c] -> X1 '*' = [c], Y1 '*'= [c], X2, X3 '+' = [c], X5 '+' = b
% [a, b, f] -> X1 '*' = f, X3 '+' = f
% [a, b, c, d] -> X1 '*' = [c, d], X4



% PidList = generate_pid_list(WhatEver),
% OldPri = process_flag(priority, high), % Raise priority, save old one
% lists:foreach(fun({Pid, Msg}) -> send_message(Pid, Msg) end, PidList),
% process_flag(priority, OldPri)