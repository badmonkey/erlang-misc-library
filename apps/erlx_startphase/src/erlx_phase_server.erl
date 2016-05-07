
-module(erlx_phase_server).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([ register_dependencies/1, defer_startup_for_later/1
        , ready_for_registration/2, cleanup_unused/1
        , service_info/1]).


-type name_type() :: atom().
         
         
%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(pid_info,
    { process                       :: pid()
    , monitor                       :: reference() | type:match_any()
    , vertex        = undefined     :: undefined | digraph:vertex() | type:match_any()
    , startmsg      = undefined     :: undefined | startup | tuple() | type:match_any()
    , server_name   = undefined     :: undefined | type:server_name() | type:match_any()
    , depends       = sets:new()    :: sets:set( name_type() ) | type:match_any()
    }).
    
-type pid_info_updater() :: fun(( #pid_info{} ) -> #pid_info{} ).    

    
-record(name_to_vertex,
    { name                          :: name_type()
    , vertex                        :: digraph:vertex()
    }).
    
   
-record(vertex_label,
    { name                          :: name_type()
    , process       = undefined     :: undefined | pid()
    , status        = dead          :: dead | inited | ready
    , notify        = true          :: boolean()
    }).
    
-type label_updater() :: fun(( #vertex_label{} ) -> #vertex_label{} ).    
    
    
-record(state,
    { deps_graph                    :: digraph:graph()
    , pidinfo                       :: ets:tid()
    , name2vertex                   :: ets:tid()    
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%


-spec register_dependencies([atom()]) -> type:ok_or_error().

register_dependencies(Names) when is_list(Names) ->
    gen_server:call(?SERVER, {wait_for_servers, self(), Names}, infinity).
    
    
-spec defer_startup_for_later(startup | tuple()) -> type:ok_or_error().
    
defer_startup_for_later(StartMsg) ->
    gen_server:call(?SERVER, {defer_for_later, self(), StartMsg}, infinity).
    
    
-spec ready_for_registration( pid(), type:server_name() ) -> type:ok_or_error().

ready_for_registration(Pid, ServerName) ->
    case gen_server_base:is_alias_name(ServerName) of
        true    -> gen_server:call(?SERVER, {ready_for_registration, Pid, ServerName}, infinity)
    ;   false   -> {error, not_a_server_name}
    end.
    
    
-spec cleanup_unused( pid() ) -> type:ok_or_error().

cleanup_unused(Pid) ->
    gen_server:call(?SERVER, {cleanup_unused_pid, Pid}, infinity).
    
    
-spec service_info( type:server_name() ) -> #vertex_label{}.

service_info(Name) ->
    gen_server:call(?SERVER, {service_info, Name}, infinity).
    
test_for_ready(V) ->
    gen_server:cast(?SERVER, {check_for_ready, V}).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    PidinfoTable = ets:new(pidinfo_table, [protected, set, named_table, {read_concurrency, true}, {keypos, 2}]),
    Name2VertexTable = ets:new(name2vertex_table, [protected, set, named_table, {read_concurrency, true}, {keypos, 2}]),
    
    { ok
    , #state{ deps_graph = digraph:new([acyclic, protected])
            , pidinfo = PidinfoTable
            , name2vertex = Name2VertexTable
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {wait_for_servers, Pid, Names}
           , _From
           , #state{} = State) ->
    case lists:all(fun(X) -> is_atom(X) end, Names) of
        true    ->
            update_pid_info(Pid,
                fun(#pid_info{} = I) ->
                    I#pid_info{ depends = sets:union(I#pid_info.depends, sets:from_list(Names)) }
                end),
            {reply, ok, State}
            
    ;   false   -> {reply, {error, badarg}, State}
    end;
    
    
handle_call( {defer_for_later, Pid, StartMsg}
           , _From
           , #state{} = State) ->
    update_pid_info(Pid,
        fun(#pid_info{} = I) ->
            I#pid_info{ startmsg = StartMsg }
        end),
    {reply, ok, State};


handle_call( {ready_for_registration, Pid, ServerName}
           , _From
           , #state{ deps_graph = G } = State) ->
    Name = gen_server_base:name(ServerName),    
    V = getcreate_named_vertex(G, Name),
    
    Info =  update_pid_info(Pid,
                fun(#pid_info{} = I) ->
                    I#pid_info{ vertex = V, server_name = ServerName }
                end),   

    update_vertex_label(G, V,
        fun(#vertex_label{} = L) ->
            L#vertex_label{ process = Pid, status = inited }
        end),
    
    process_depends(G, V, sets:to_list(Info#pid_info.depends)),
    
    test_for_ready(V),
    {reply, ok, State};
    
    
handle_call( {cleanup_unused_pid, Pid}
           , _From
           , #state{} = State) ->  
    case get_pid_info(Pid) of
        false               -> {reply, {error, invalid_pid}, State}
    ;   #pid_info{} = Info  ->
            case Info#pid_info.vertex of
                undefined   ->
                    xets:match_delete(pidinfo_table, #pid_info{ process = Pid, _ = '_' }),
                    {reply, ok, State}
                    
            ;   _           ->
                    {reply, {error, pid_in_use}, State}
            end
            
    end;


handle_call( {service_info, Name}
           , _From
           , #state{ deps_graph = G } = State) ->  
    case gen_server_base:is_alias_name(Name) of
        true    ->
            case ets:lookup(name2vertex_table, Name) of
                []                          -> {reply, false, State}
            ;   [{name_to_vertex, Name, V}] ->
                    case get_vertex_label(G, V) of
                        false   -> {reply, {error, unlabelled_vertex}, State}
                    ;   L       -> {reply, L, State}
                    end
            end
            
    ;   false   ->
            {reply, {error, badarg}, State}
    end;
    

handle_call(_Request, _From, State) ->
    lager:info("erlx_phase_server:call stopped ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_cast( {check_for_ready, V}
           , #state{ deps_graph = G } = State) ->
    case digraph:vertex(G, V) of
        false                                       ->
            {stop, {error, unlabelled_vertex}, State}
            
    ;   {V, #vertex_label{ status = inited } = L }  ->
            case all_dependencies_are_ready(G, V) of
                false   -> ok
            ;   true    ->
                    Target = L#vertex_label.process,
                    case get_pid_info(Target) of
                        false   -> ok
                    ;   Info    ->
                            digraph:add_vertex(G, V, L#vertex_label{ status = ready }),
                            
                            case Info#pid_info.startmsg of
                                undefined   -> ok
                            ;   Msg         -> erlang:send(Target, Msg)
                            end,
                            
                            gen_server_base:register_name(Info#pid_info.server_name, Target),
                            lists:foreach( fun(X) -> test_for_ready(X) end
                                         , digraph:out_neighbours(G, V)),
                                         
                            notify_up(G, V, L#vertex_label.name)
                    end
            end,
            {noreply, State}
            
    ;   _                                           ->
            {noreply, State}
    end;

    
handle_cast(_Msg, State) ->
    lager:info("erlx_phase_server:cast stopped ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( {'DOWN', _Ref, process, Pid, _Reason}
           , #state{ deps_graph = G } = State) ->
    case get_pid_info(Pid) of
        false               -> {stop, unknown_process, State}
    ;   #pid_info{} = Info  ->
            case Info#pid_info.vertex of
                undefined   -> ok
            ;   V           ->
                    Label = update_vertex_label(G, V,
                                fun(L) ->
                                    L#vertex_label{ process = undefined, status = dead }
                                end),
                    notify_down(G, V, Label#vertex_label.name)
            end,
            xets:match_delete(pidinfo_table, #pid_info{ process = Pid, _ = '_' }),
            {noreply, State}
    end;
           
    
handle_info(_Info, State) ->
    lager:info("erlx_phase_server:info stopped ~p", [_Info]),
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


-spec get_pid_info( undefined | pid() ) -> false | #pid_info{}.

get_pid_info(undefined) -> false;
get_pid_info(Pid) ->
    case ets:lookup(pidinfo_table, Pid) of
        []                  -> false
    ;   [#pid_info{} = PI]  -> PI
    end.


-spec getcreate_pid_info( pid() ) -> #pid_info{}.

getcreate_pid_info(Pid) ->
    case ets:lookup(pidinfo_table, Pid) of
        []                  ->
            Ref = erlang:monitor(process, Pid),
            #pid_info{ process = Pid
                     , monitor = Ref }
            
    ;   [#pid_info{} = PI]  -> PI
    end.


-spec update_pid_info( pid(), pid_info_updater() ) -> #pid_info{}.

update_pid_info(Pid, Updater) ->
    Info = getcreate_pid_info(Pid),
    Info2 = Updater(Info),
    ets:insert(pidinfo_table, Info2),
    Info2.

    
%%%%% ------------------------------------------------------- %%%%%


-spec getcreate_named_vertex( digraph:graph(), name_type() ) -> digraph:vertex().

getcreate_named_vertex(G, Name) ->
    case ets:lookup(name2vertex_table, Name) of
        []                              ->
            V = digraph:add_vertex(G),
            digraph:add_vertex(G, V, #vertex_label{ name = Name }),
            ets:insert(name2vertex_table, #name_to_vertex{ name = Name, vertex = V }),
            V
            
    ;   [{name_to_vertex, Name, V}]     -> V
    end.


%%%%% ------------------------------------------------------- %%%%%
    
    
-spec get_vertex_label( digraph:graph(), digraph:vertex() ) -> false | #vertex_label{}.
    
get_vertex_label(G, V) ->
    case digraph:vertex(G, V) of
        false       -> false
    ;   {V, Label}  -> Label
    end.


-spec update_vertex_label( digraph:graph(), digraph:vertex(), label_updater() ) -> type:exception() | #vertex_label{}.

update_vertex_label(G, V, Updater) ->
    case get_vertex_label(G, V) of
        false   -> throw({error, unlabelled_vertex})
    ;   L       ->
            L2 = Updater(L),
            digraph:add_vertex(G, V, L2),
            L2
    end.
    

%%%%% ------------------------------------------------------- %%%%%


process_depends(_, _, [])           -> ok;
process_depends(G, V, [X | Tail])   ->
    V2 = getcreate_named_vertex(G, X),
    
    case xdigraph:find_edge(G, V2, V) of
        false   -> digraph:add_edge(G, V2, V)   % todo check for bad edge errors (cycles et al)
    ;   _       -> ok
    end,
    process_depends(G, V, Tail).
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

-spec all_dependencies_are_ready( digraph:graph(), digraph:vertex() ) -> boolean().

all_dependencies_are_ready(G, V) ->
    lists:all( fun(X) -> is_vertex_ready(G, X) end
             , digraph:in_neighbours(G, V) ).


-spec is_vertex_ready( digraph:graph(), digraph:vertex() ) -> boolean().

is_vertex_ready(G, V) ->
    case digraph:vertex(G, V) of
        {V, #vertex_label{ status = ready }}    -> true
    ;   _                                       -> false
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


notify_up(G, V, Name) ->
    ok.
    
    
notify_down(G, V, Name) ->
    ok.
    
    
    