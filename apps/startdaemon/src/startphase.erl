
-module(startphase).

-export([ start_link_name/3, start_link/3, start_link/4
        , register_started_service/2
        , required/1, defer_startup/1, defer_startup/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec start_link_name( type:server_name(), module(), term() ) -> type:start_result().

start_link_name(Name, Module, Args) ->
    case Name of
        undefined           -> start_link(Module, Args, [])
    ;   _                   -> start_link(Name, Module, Args, [])
    end.
    

-spec start_link( module(), term(), type:properties() ) -> type:start_result().

start_link(Module, Args, Options) ->
    Result = gen_server:start_link(Module, Args, property:as_proplists(Options)),
    register_started_service(undefined, Result).


-spec start_link( type:server_name(), module(), term(), type:properties() ) -> type:start_result().
    
start_link(ServerName, Module, Args, Options) ->
    Result = gen_server:start_link(Module, Args, property:as_proplists(Options)),
    register_started_service(ServerName, Result).
    
    

-spec register_started_service( undefined | type:server_name(), type:start_result() ) -> type:start_result().

register_started_service(undefined, StartLinkResult) ->
    case StartLinkResult of
        {ok, Pid}   ->
            erlx_phase_server:cleanup_unused(Pid),
            {ok, Pid}
            
    ;   _ = Err     -> Err
    end;
    
register_started_service(ServerName, StartLinkResult) ->
    case StartLinkResult of
        {ok, Pid}   ->
            case erlx_phase_server:ready_for_registration(Pid, ServerName) of
                ok                  -> {ok, Pid}
            ;   {error, _} = Err    -> Err
            end
            
    ;   _ = Err     -> Err
    end.
    

%%%%% ------------------------------------------------------- %%%%%


required(Name) when is_atom(Name) ->
    erlx_phase_server:register_dependencies([Name]);
    
    
required(Names) when is_list(Names) ->
    erlx_phase_server:register_dependencies(Names).


%%%%% ------------------------------------------------------- %%%%%


defer_startup(State) ->
    defer_startup(State, startup).
    
    
defer_startup({error,_} = E, _)         ->
    {stop, E};

defer_startup(State, StartupMessage)    ->
    ok = erlx_phase_server:defer_startup_for_later(StartupMessage),
    {ok, State}.

