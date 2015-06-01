
-module(router).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([new/0, new/1, free/1, add/3, remove/2, get/2, get/3, get_with_matchs/2, get_with_matchs/3]).
-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

                  

-type nodeid() :: pos_integer().
-type routerid() :: term().
-type data() :: term().

-type path_fragment() :: string() | binary() | atom() | number().
-type match_fragment() :: match_star | {match_star, atom()}
                        | match_one | {match_one, atom()} | {match_one, atom(), {term(), term()}}.
-type value_fragment() :: {atom(), path_fragment()}.
            
-type path() :: [path_fragment() | match_fragment()].
-type bound_path() :: [path_fragment() | value_fragment()].

-export_type([nodeid/0, routerid/0, data/0, path_fragment/0, match_fragment/0, value_fragment/0, path/0, bound_path/0]).

         

%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(router,
    { router                        :: routerid()
    , rootnode                      :: nodeid()
    }).

-record(route_node,
    { key                           :: nodeid()
    , router                        :: routerid()
    , data                          :: [data()]
    , children      = dict:new()    :: dict:dict( path_fragment(), nodeid() )
    }).
    

-record(state,
    { router_table                  :: ets:tid()
    , node_table                    :: ets:tid()
    , nextid        = 1             :: routerid()
    , registry      = dict:new()    :: dict:dict( atom(), routerid() )
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).



-spec new() -> routerid() | type:error().
new() ->
    gen_server:call(?SERVER, {new_router}).
    
    
-spec new( atom() ) -> routerid() | type:error().    
new(Name) ->
    gen_server:call(?SERVER, {new_router, Name}).    
    
    
-spec free( routerid() ) -> type:ok_or_error().    
free(Router) ->
    gen_server:call(?SERVER, {free_router, Router}).

    
    
-spec add( routerid(), path(), data() ) -> type:ok_or_error().
add(Router, Path, Data) ->
    gen_server:call(?SERVER, {add_route, Router, Path, Data}).
    

-spec remove( routerid(), path() ) -> type:ok_or_error().    
remove(Router, Path) ->
    gen_server:call(?SERVER, {remove_route, Router, Path}).
    

%%%%% ------------------------------------------------------- %%%%%
    
   
-spec get( routerid(), path() ) -> [data()].
get(Router, Path) ->
    ok.
    
    
-spec get( routerid(), path(), term() ) -> [data()].
get(Router, Path, Args) ->
    ok.
    
    
    
-spec get_with_matchs( routerid(), path() ) -> [{data(), bound_path()}].
get_with_matchs(Router, Path) ->
    ok.


-spec get_with_matchs( routerid(), path(), term() ) -> [{data(), bound_path()}].
get_with_matchs(Router, Path, Args) ->
    ok.

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    RouterTable = ets:new(router_table, [protected, set, {read_concurrency, true}, {keypos, 2}]),
    NodeTable = ets:new(node_table, [protected, set, {read_concurrency, true}, {keypos, 2}]),
    
    {ok, #state{ router_table = RouterTable
               , node_table = NodeTable
               }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({new_router}, _From, State) ->
    {reply, ok, State};
    
    
handle_call({new_router, Name}, _From, State) ->
    {reply, ok, State};    
    
    
handle_call({free_router, Router}, _From, State) ->
    {reply, ok, State};

    
handle_call({add_route, Router, Path, Data}, _From, State) ->
    {reply, ok, State};
    
    
handle_call({remove_route, Router, Path}, _From, State) ->
    {reply, ok, State};
    
    
handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions




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