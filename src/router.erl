
-module(router).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([wait_for_service/0, exists/1, new/0, new/1, free/1, add/3, remove/3]).
-export([get/2, get_many/2, get_with_matches/2]).
-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

                  

-type nodeid() :: pos_integer().
-type routerid() :: pos_integer().
-type data() :: term().

-type routername() :: atom() | routerid().

-type path_fragment() :: string() | binary() | atom() | number().
-type match_fragment() :: match_one | match_star.
-type value_fragment() :: {match_one, path_fragment()} | {match_star, publish_path()}.

-type subscribe_fragment() :: path_fragment() | match_fragment().
            
-type subscribe_path() :: [subscribe_fragment()].
-type publish_path() :: [path_fragment()].

-type value_list() :: [value_fragment()].
-type match_list() :: [ { data(), value_list() } ].


-export_type([ nodeid/0, routerid/0, data/0, routername/0
             , path_fragment/0, match_fragment/0, value_fragment/0, subscribe_fragment/0
             , subscribe_path/0, publish_path/0, value_list/0, match_list/0]).

         

%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(router_root,
    { id                            :: routerid()
    , rootnode                      :: type:searchable( nodeid() )
    }).

-record(route_node,
    { key                           :: type:searchable( nodeid() )
    , router                        :: routerid()
    , data          = []            :: type:searchable( [data()] )
    , children      = dict:new()    :: type:searchable( dict:dict( subscribe_fragment(), nodeid() ) )
    }).
    
-record(registry,
    { name                          :: type:searchable( atom() )
    , router                        :: routerid()
    }).
    

-record(state,
    { root_table                    :: ets:tid()
    , node_table                    :: ets:tid()
    , registry_table                :: ets:tid()
    , nextid        = 1             :: routerid()
    , nextnode      = 1             :: nodeid()
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


wait_for_service() ->
    gen_server:call(?SERVER, {wait_for_service}).


-spec new() -> routerid() | type:error().

new() ->
    gen_server:call(?SERVER, {new_router}).
    
    
-spec new( atom() ) -> routerid() | type:error().    

new(Name) ->
    gen_server:call(?SERVER, {new_router, Name}).    
    
    
-spec free( routername() ) -> type:ok_or_error().    

free(Router) ->
    gen_server:call(?SERVER, {free_router, Router}).

    
    
-spec add( routername(), subscribe_path(), data() ) -> type:ok_or_error().

add(Router, Path, Data) ->
    gen_server:call(?SERVER, {add_route, Router, Path, Data}).


-spec remove( routername(), subscribe_path(), data() ) -> type:ok_or_error().
    
remove(Router, Path, Data) ->
    gen_server:call(?SERVER, {add_route, Router, Path, Data}).


%%%%% ------------------------------------------------------- %%%%%
% Public API that doesn't require talking to ?SERVER
    
    
-spec exists( routername() ) -> boolean().

exists(Router) ->
    get_router(Router) =/= undefined.
    
   
-spec get( routername(), publish_path() ) -> [data()] | no_return().

get(Router, Path) ->
    gather([{get_root_node(Router), Path}], []).

    
-spec get_many( [routername()], [publish_path()] ) -> [data()] | no_return().

get_many(Routers, Paths) ->
    gather([ {get_root_node(X), Y} || X <- Routers, Y <- Paths ], []).
    
    
-spec get_with_matches( routername(), [publish_path()] ) -> match_list() | no_return().

get_with_matches(Router, Paths) ->
    gather_matches([{get_root_node(Router), X, []} || X <- Paths], []).    

    
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


handle_call({wait_for_service}, _From, State) ->
    {reply, ok, State};

    
handle_call({new_router}, _From, State) ->
    Id = State#state.nextid,
    Node = State#state.nextnode,
    
    ets:insert(State#state.root_table, #router_root{ id = Id, rootnode = Node } ),
    ets:insert(State#state.node_table, #route_node{ key = Node, router = Id } ),
    
    {reply, Id, State#state{ nextid = Id + 1, nextnode = Node + 1 }};
    
    
handle_call({new_router, Name}, From, State)
        when is_atom(Name)  ->
    {reply, Id, NewState} = handle_call({new_router}, From, State),
    ets:insert(State#state.registry_table, #registry{ name = Name, router = Id } ),
    
    {reply, Id, NewState}; 
    
    
handle_call({free_router, Router}, _From, State) ->
    Id = get_router(Router),
   
    xets:match_delete(State#state.root_table, #router_root{ id = Id, _ = '_'}),
    xets:match_delete(State#state.node_table, #route_node{ router = Id, _ = '_' }),
    xets:match_delete(State#state.registry_table, #registry{ router = Id, _ = '_' }),
    
    {reply, ok, State};

    
handle_call({add_route, Router, Path, Data}, _From, State) ->
    {Top, NewState} = lists:foldl(
                            fun(Frag, {CurrentNode, NState}) ->
                                case get_next_node(CurrentNode, Frag) of
                                    undefined   ->
                                        make_new_node(CurrentNode, Frag, NState)
                                ;   X           -> {X, NState}
                                end
                            end,
                            {get_root_node(Router), State},
                            Path),
    ets:insert(State#state.node_table, Top#route_node{ data = [Data | Top#route_node.data] }),
    {reply, ok, NewState};
    
    
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


-spec get_router( routername() ) -> routerid() | undefined.

get_router(Name)
        when is_atom(Name)  ->
    case ets:lookup(router_registry_table, Name) of
        []                      -> undefined
    ;   [{registry, Name, Id}]  -> Id
    end;
get_router(Id) -> Id.


-spec get_root_node( routername() ) -> #route_node{} | no_return().   
 
get_root_node(Router) ->
    Id = get_router(Router),
    case ets:lookup(router_root_table, Id) of
        []                      -> throw({error, {invalid_router, Router}})
    ;   [{router, Id, Nodeid}]  ->
            case ets:lookup(router_node_table, Nodeid) of
                []                      -> throw({error, {invalid_node, Nodeid}})
            ;   [#route_node{} = Node]  -> Node
            end
    end.


%%%%% ------------------------------------------------------- %%%%%

    
-spec make_new_node( #route_node{}, subscribe_fragment(), #state{} ) -> { #route_node{}, #state{} }.    

make_new_node(#route_node{ router = Router, children = Children } = Node, Frag, State) ->
    NodeId = State#state.nextnode,
    NewNode = #route_node{ key = NodeId, router = Router },
    ets:insert(State#state.node_table, NewNode),
    ets:insert(State#state.node_table, Node#route_node{ children = dict:store(Frag, NodeId, Children) }),
    { NewNode, State#state{ nextnode = NodeId +1 } }.

    
%%%%% ------------------------------------------------------- %%%%%

    
-spec gather( [{#route_node{}, publish_path()}], [data()] ) -> [data()].
    
gather([], Acc) -> Acc; 
gather([{undefined, _} | Rest], Acc) ->
    gather(Rest, Acc);
    
gather([{#route_node{} = Node, []} | Rest], Acc) ->
    gather(Rest, Acc ++ Node#route_node.data);

gather([{#route_node{} = Node, [Hd | Tail]} | Rest], Acc) ->
    NewList = [ {get_next_node(Node, Hd), Tail}
              , {get_next_node(Node, match_one), Tail}
              , {get_next_node(Node, match_any), []}
              | Rest],
    gather(NewList, Acc).
    
    
%%%%% ------------------------------------------------------- %%%%%

    
-spec gather_matches( [{#route_node{}, publish_path(), value_list()}], match_list() ) -> match_list().
    
gather_matches([], Acc) -> Acc; 
gather_matches([{undefined, _, _} | Rest], Acc) ->
    gather_matches(Rest, Acc);
    
gather_matches([{#route_node{} = Node, [], Values} | Rest], Acc) ->
    Fixed = lists:reverse(Values),
    gather_matches(Rest, Acc ++ [ {D, Fixed} || D <- Node#route_node.data]);

gather_matches([{#route_node{} = Node, [Hd | Tail] = Path, Values} | Rest], Acc) ->
    NewList = [ {get_next_node(Node, Hd), Tail, [Hd | Values]}
              , {get_next_node(Node, match_one), Tail, [{match_one, Hd} | Values]}
              , {get_next_node(Node, match_any), [], [{match_any, Path} | Values]}
              | Rest],
    gather_matches(NewList, Acc).    


%%%%% ------------------------------------------------------- %%%%%

    
get_next_node( #route_node{ children = Children }, Frag ) ->
    case dict:find(Frag, Children) of
        {ok, Value} ->
            case ets:lookup(router_node_table, Value) of
                []                      -> undefined
            ;   [#route_node{} = Node]  -> Node
            end
    ;   error       -> undefined
    end.

