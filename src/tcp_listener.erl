
-module(tcp_listener).

-behaviour(gen_server).


%% External API
-export([start_link/2, start_link/3, start_link/4]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).


     
%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        :: atom()       % callback module
    , proxystate                    :: term()       % state of callback module
    , addrs         = dict:new()    :: dict:dict(inet:socket(), {type:endpoint(), term()})
    , sockets       = dict:new()    :: dict:dict(type:endpoint(), inet:socket())
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-type listener_type() :: {Endpt :: type:endpoint(), UserData :: term()}.
-type connection_type() :: {Local :: type:endpoint(), Remote :: type:endpoint(), Socket :: inet:socket()}.

-callback handle_connection(Connection :: connection_type(), UserData :: term(), State :: term()) ->
      type:start_result()
    | {ok, Pid :: pid(), State1 :: term()}
    | {ignore, State1 :: term()}
    | {stop, Reason :: term(), State1 :: term()}.
    
-callback handle_error(Info :: listener_type() | connection_type(), Reason :: term(), State :: term()) ->
      {stop, Reason1 :: term(), State1 :: term()}
    | {noreply, State1 :: term()}.

    
% gen_server callbacks

-callback init(Args :: term()) ->
      {ok, State :: term()}
    | {ok, State :: term(), timeout()}
    | hibernate | ignore
    | {stop, Reason :: term()}.
    
-callback handle_call( Request :: term()
                     , From :: {pid(), Tag :: term()}
                     , State :: term()) ->
      {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} 
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_cast(Request :: term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_info(Info :: timeout | term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback terminate( Reason :: (normal | shutdown | {shutdown, term()} | term())
                   , State :: term()) ->
    term().
    
-callback code_change( OldVsn :: (term() | {down, term()})
                     , State :: term()
                     , Extra :: term()) ->
      {ok, NewState :: term()}
    | {error, Reason :: term()}.


%%%%% ------------------------------------------------------- %%%%%


start_link(CallbackModule, Port)
        when is_atom(CallbackModule), is_integer(Port)  ->
    start_link(CallbackModule, undefined, [], [{localhost, Port, undefined}]).


start_link(CallbackModule, Port, InitParams)
        when is_atom(CallbackModule), is_integer(Port), is_list(InitParams)  ->
    start_link(CallbackModule, undefined, InitParams, [{localhost, Port, undefined}]);
    
start_link(CallbackModule, Name, Port)
        when is_atom(CallbackModule), is_integer(Port)  ->
    start_link(CallbackModule, Name, [], [{localhost, Port, undefined}]);
    
start_link(CallbackModule, Port, UserData)
        when is_atom(CallbackModule), is_integer(Port)  ->
    start_link(CallbackModule, undefined, [], [{localhost, Port, UserData}]);
    
start_link(CallbackModule, IpAddr, Port)
        when is_atom(CallbackModule), is_tuple(IpAddr), is_integer(Port)  ->
    start_link(CallbackModule, undefined, [], [{IpAddr, Port, undefined}]).


start_link(CallbackModule, Name, Port, InitParams)
        when is_atom(CallbackModule), is_integer(Port), is_list(InitParams)  ->
    start_link(CallbackModule, Name, InitParams, [{localhost, Port, undefined}]);
    
start_link(CallbackModule, IpAddr, Port, UserData)
        when is_atom(CallbackModule), is_tuple(IpAddr), is_integer(Port)  ->
    start_link(CallbackModule, undefined, [], [{IpAddr, Port, UserData}]);


%%  
%% ListenerList = [{IpAddr, Port, Userdata}]
%%
start_link(CallbackModule, Name, InitParams, ListenerList)
        when  is_atom(CallbackModule)
            , is_atom(Name) orelse is_tuple(Name)
            , is_list(InitParams), is_list(ListenerList)  ->
    gen_server_base:start_link_name(Name, ?MODULE, [CallbackModule, InitParams, ListenerList]).
    

%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, InitParams, Listeners]) ->
    process_flag(trap_exit, true),
    
    behaviour:assert(CallbackModule, tcp_listener),

    InitState = #state{module = CallbackModule},

    case catch CallbackModule:init(InitParams) of
        {ok, ProxyState}        ->
            start_all_listeners(Listeners, InitState#state{proxystate = ProxyState}, undefined)
    ;   {ok, ProxyState, Arg}   ->
            start_all_listeners(Listeners, InitState#state{proxystate = ProxyState}, Arg)
    ;   {stop, _} = Stop        -> Stop
    ;   ignore                  -> ignore
    ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
    ;   Else                    -> {stop, {bad_return_value, Else}}
    end.
   
   
-define(REQUIRED_SOCK_OPTS, [binary, {packet, raw}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}]).   
   
start_all_listeners(ListenerList, State0, Arg) ->
    StateN = lists:foldl(
                fun (_Listener, {error, _} = Err)   -> Err
                    
                ;   ({IpAddr, Port, UserData}, #state{} = State)    ->
                        {NIpAddr, NSockOpts} =  case IpAddr of
                                                    localhost           -> { {0,0,0,0}, ?REQUIRED_SOCK_OPTS }
                                                ;   {0,0,0,0}           -> { IpAddr, ?REQUIRED_SOCK_OPTS }
                                                ;   {_,_,_,_}           -> { IpAddr, [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS] }
                                                ;   {_,_,_,_,_,_,_,_}   -> { IpAddr, [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS] }
                                                ;   _                   -> { {0,0,0,0}, ?REQUIRED_SOCK_OPTS }
                                                end,

                        NextState = start_listener(NIpAddr, Port, UserData, NSockOpts, State),

                        case NextState of
                            {error, Reason}     ->
                                Endpoint = {IpAddr, Port},
                                case process_error({Endpoint, UserData}, {start_listener, Reason}, State) of
                                    {noreply, NewPState}    -> State#state{proxystate = NewPState}
                                ;   {stop, ReasonHE, _}     -> {error, ReasonHE}
                                end
                            
                        ;   _                   -> NextState
                        end
                        
                end, State0, ListenerList),
                
    case {StateN, Arg} of
        {{error,Reason}, _} -> {stop, Reason}
        
    ;   {_, undefined}      -> {ok, StateN}
    ;   _                   -> {ok, StateN, Arg}
    end.


%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_call(Request, From, ProxyState) of
        {reply, Reply, NewServerState}          -> {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply, Reply, NewServerState, Arg}     -> {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply, NewServerState}               -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}          -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}          -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {stop, Reason, Reply, NewServerState}   -> {stop, Reason, Reply, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                        -> {stop, {error, Reason}, State}
    ;   Else                                    -> {stop, {bad_return_value, Else}, State}
    end.


%%%%% ------------------------------------------------------- %%%%%


%handle_cast({shutdown, Reason}, State) ->
%    gen_server:terminate({shutdown, Reason}, State).
    
handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                -> {stop, {error, Reason}, State}
    ;   Else                            -> {stop, {bad_return_value, Else}, State}
    end.
    

%%%%% ------------------------------------------------------- %%%%%


handle_info( {inet_async, ListenSock, _Ref, {ok, ClientSocket}}
           , #state{module = CallbackModule, proxystate = ProxyState, addrs = Addresses} = State) ->
    try
        {ok, {Endpoint, UserData}} = dict:find(ListenSock, Addresses),
        {ok, Remote} = inet:peername(ClientSocket),
        
        case transfer_sockopt(ListenSock, ClientSocket) of
            ok                  ->
                % TODO handle exception
                case catch CallbackModule:handle_connection({Endpoint, Remote, ClientSocket}, UserData, ProxyState) of
                    {ok, Pid}           ->
                        gen_tcp:controlling_process(ClientSocket, Pid),
                        {noreply, create_async_acceptor(ListenSock, State)}
                    
                ;   ignore              ->
                        gen_tcp:close(ClientSocket),
                        {noreply, State}
                        
                ;   {error, ReasonHC}   ->
                        process_error({Endpoint, Remote, ClientSocket}, {handle_connection, ReasonHC}, State)
                
                ;   {ok, Pid, NewState} ->
                        gen_tcp:controlling_process(ClientSocket, Pid),
                        {noreply, create_async_acceptor(ListenSock, State#state{proxystate = NewState})}
                        
                ;   {ignore, NewState}  ->
                        gen_tcp:close(ClientSocket),
                        {noreply, State#state{proxystate = NewState}}
                
                ;   {stop, Reason, NewState}  ->
                        {stop, Reason, State#state{proxystate = NewState}}
                
                ;   {'EXIT', Reason}    ->
                        {stop, {error, Reason}, State}
                        
                ;   Else                ->
                        {stop, {bad_return_value, Else}, State}
                end
                
        ;   {error, ReasonSO}   ->
                process_error({Endpoint, UserData}, {copy_sockopts, ReasonSO}, State)                
        end

    catch exit:Why ->
        {stop, Why, State}
    end;

    
handle_info( {inet_async, ListenSock, _Ref, Error}
           , #state{addrs = Addresses} = State)  ->
   
    {ok, {Endpoint, UserData}} = dict:find(ListenSock, Addresses),
    
    process_error({Endpoint, UserData}, {async_accept, Error}, State);


%
%% Passthrough
%

handle_info(Info, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_info(Info, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                -> {stop, {error, Reason}, State}
    ;   Else                            -> {stop, {bad_return_value, Else}, State}
    end.


%%%%% ------------------------------------------------------- %%%%%

trace_close(Sock) ->
    xerlang:trace({"TERMINATE", Sock}),
    gen_tcp:close(Sock).

terminate(Reason, #state{module = CallbackModule, proxystate = ProxyState, addrs = Addresses}) ->
    %[gen_tcp:close(Sock) || Sock <- dict:fetch_keys(Addresses)],
    [trace_close(Sock) || Sock <- dict:fetch_keys(Addresses)],
    catch CallbackModule:terminate(Reason, ProxyState),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
    

% return :: #state{} | {error, Reason}
start_listener(IpAddr, Port, UserData, TcpOpts, #state{addrs = Addresses, sockets = Sockets} = State0) ->
    Endpoint = {IpAddr, Port},
    case dict:find(Endpoint, Sockets) of
        {ok, _} -> State0 % should be an error?
    ;   error   ->
            case gen_tcp:listen(Port, TcpOpts) of
                {ok, NewListenSocket} ->
                    xerlang:trace("Created new socket"),
                    State1 = State0#state{
                                      addrs   = dict:store(NewListenSocket, {Endpoint, UserData}, Addresses)
                                    , sockets = dict:store(Endpoint, NewListenSocket, Sockets)
                                },
                    create_async_acceptor(NewListenSocket, State1)
                    
            ;   {error, _} = Err -> Err
            end
    end.
    

create_async_acceptor(ListenSocket, #state{} = State) ->
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, _NewRef}   -> State
    ;   {error, NewRef} -> {error, inet:format_error(NewRef)}
    end.
    

-define(COPY_SOCK_OPTS, [active, nodelay, keepalive, delay_send, priority, tos, sndbuf]).
    
transfer_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, ?COPY_SOCK_OPTS) of
        {ok, TcpOpts} ->
            case prim_inet:setopts(ClientSocket, TcpOpts) of
                ok    -> ok
            ;   Error ->
                    gen_tcp:close(ClientSocket),
                    Error
            end
            
    ;   Error ->
            gen_tcp:close(ClientSocket),
            Error
    end.

    
process_error( ConnInfo, Error
             , #state{module = CallbackModule, proxystate = ProxyState} = State)  ->
             
    case catch CallbackModule:handle_error(ConnInfo, Error, ProxyState) of
        {stop, Reason, NewState}    -> {stop, Reason, State#state{proxystate = NewState}}
    ;   {noreply, NewState}         -> {noreply, State#state{proxystate = NewState}}
    ;   {'EXIT', Reason}            -> {stop, {error, Reason}, State}
    ;   Else                        -> {stop, {bad_return_value, Else}, State}
    end.

    