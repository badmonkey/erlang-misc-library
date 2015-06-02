
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
-type routerid() :: pos_integer().
-type data() :: term().

-type routername() :: atom() | routerid().

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
    { id                            :: routerid()
    , rootnode                      :: nodeid()
    }).

-record(route_node,
    { key                           :: nodeid()
    , router                        :: routerid()
    , data          = []            :: [data()]
    , children      = dict:new()    :: dict:dict( path_fragment(), nodeid() )
    }).
    
-record(registry,
	{ name							:: atom()
	, router						:: routerid()
	}).
    

-record(state,
    { root_table                    :: ets:tid()
    , node_table                    :: ets:tid()
    , registry_table				:: ets:tid()
    , nextid        = 1             :: routerid()
    , nextnode      = 1             :: nodeid()
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
    
    
-spec free( routername() ) -> type:ok_or_error().    
free(Router) ->
    gen_server:call(?SERVER, {free_router, Router}).

    
    
-spec add( routername(), path(), data() ) -> type:ok_or_error().
add(Router, Path, Data) ->
    gen_server:call(?SERVER, {add_route, Router, Path, Data}).
    

-spec remove( routername(), path() ) -> type:ok_or_error().    
remove(Router, Path) ->
    gen_server:call(?SERVER, {remove_route, Router, Path}).
    
    

%%%%% ------------------------------------------------------- %%%%%
    
   
-spec get( routername(), path() ) -> [data()].
get(Router, Path) ->
    ok.
    
    
-spec get( routername(), path(), term() ) -> [data()].
get(Router, Path, Args) ->
    ok.
    
    
    
-spec get_with_matchs( routername(), path() ) -> [{data(), bound_path()}].
get_with_matchs(Router, Path) ->
    ok.


-spec get_with_matchs( routername(), path(), term() ) -> [{data(), bound_path()}].
get_with_matchs(Router, Path, Args) ->
    ok.

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    RootTable = ets:new(router_root_table, [protected, set, named_table, {read_concurrency, true}, {keypos, 2}]),
    NodeTable = ets:new(router_node_table, [protected, set, named_table, {read_concurrency, true}, {keypos, 2}]),
    RegTable  = ets:new(router_registry_table, [protected, set, named_table, {read_concurrency, true}, {keypos, 2}]),
    
    {ok, #state{ root_table = RootTable
               , node_table = NodeTable
               , registry_table = RegTable
               }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({new_router}, _From, State) ->
	Id = State#state.nextid,
	Node = State#state.nextnode,
	
	ets:insert(State#state.root_table, #router{ id = Id, rootnode = Node } ),
	ets:insert(State#state.node_table, #route_node{ key = Node, router = Id } ),
	
    {reply, Id, State#state{ nextid = Id + 1, nextnode = Node + 1 }};
    
    
handle_call({new_router, Name}, From, State)
		when is_atom(Name)  ->
	{reply, Id, NewState} = handle_call({new_router}, From, State),
	ets:insert(State#state.registry_table, #registry{ name = Name, router = Id } ),
	
    {reply, Id, NewState}; 
    
    
handle_call({free_router, Router}, _From, State) ->
	Id = get_router(Router),

	match_delete(State#state.root_table, #router{ id = Id, _ = '_' }),
	match_delete(State#state.node_table, #route_node{ router = Id, _ = '_' }),
	match_delete(State#state.registry_table, #registry{ router = Id, _ = '_' }),
	
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


terminate(_Reason, State) ->
	ets:delete(State#state.root_table),
	ets:delete(State#state.node_table),
	ets:delete(State#state.registry_table),
    ok.

    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


-spec get_router( routername() ) -> routerid().
get_router(Name)
		when is_atom(Name)  ->
	case ets:lookup(router_registry_table, Name) of
		[]				-> throw({unknown_router, Name})
	;	[{Name, Id}]	-> Id
	end;
	
	
get_router(Id) -> Id.



match_delete(Table, Spec) ->
	[ ets:delete_object(Table, X) || X <- ets:match_object(Table, Spec) ].

	
	
gather([], Acc) -> Acc;	
gather([{undefined, _} | Rest], Acc) ->
	gather(Rest, Acc);
	
gather([{#route_node{} = Node, []} | Rest], Acc) ->
	gather(Rest, Acc ++ Node#route_node.data);

gather([{#route_node{} = Node, [Hd | Tail]} | Rest], Acc) ->
	NewList = [ {get_next_node(Node, Hd), Tail}
			  , {get_next_node(Node, match_one), Tail}
			  , {get_next_node(Node, match_any), []}
			  , Rest],
	gather(NewList, Acc).
	

	
get_next_node( #route_node{ children = Children }, Frag ) ->
	case dict:find(Frag, Children) of
		{ok, Value}	->
			case ets:lookup(router_node_table, Value) of
				[]						-> undefined
			;	[#route_node{} = Node]	-> Node
			end
	;	error		-> undefined
	end.



-spec is_wildcard( path_fragment() | match_fragment() ) -> boolean().

is_wildcard(match_star) -> true;
is_wildcard({match_star,_}) -> true;
is_wildcard(match_one) -> true;
is_wildcard({match_one,_}) -> true;
is_wildcard({match_one,_,{_,_}}) -> true;
is_wildcard(_) -> false.



% PidList = generate_pid_list(WhatEver),
% OldPri = process_flag(priority, high), % Raise priority, save old one
% lists:foreach(fun({Pid, Msg}) -> send_message(Pid, Msg) end, PidList),
% process_flag(priority, OldPri)