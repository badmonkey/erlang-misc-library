
-module(erlx_tcp_server).

-behaviour(gen_server).


%% External API
-export([start_link/2, start_link/3, start_link/4]).
-export([behaviour_info/1]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).


% Same behaviour as gen_server plus addition api functions
behaviour_info(callbacks) ->
    [ {handle_connection, 1}, {handle_error, 3}
    , {init, 1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}
    , {terminate, 2}, {code_change, 3} ];
behaviour_info(_) -> undefined.


-define(ERLX_TCP_SERVER_SYSTEM, '$erlx_tcp_server').
-define(PACKET_PROCESSOR_NONE, '$erlx_tcp_server$packet_none').
-define(COPY_SOCK_OPTS, [active, nodelay, keepalive, delay_send, priority, tos, sndbuf]).
-define(REQUIRED_SOCK_OPTS, [binary, {packet, raw}]).

     
%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        % callback module
    , proxystate                    % state of callback module
    , addrs         = dict:new()    % socket -> {ipaddr, port, userdata}
    , sockets       = dict:new()    % {ipaddr, port} -> socket
    }).

    
%%%%% ------------------------------------------------------- %%%%%


%start_listener
%stop_listener(IpAddr, Port)
%stop_listener(Socket)


%%%%% ------------------------------------------------------- %%%%%


start_link(CallbackModule, Port)
        when is_atom(CallbackModule), is_integer(Port) ->
    start_link(CallbackModule, undefined, [], [{undefined, Port, undefined}]).


start_link(CallbackModule, Port, InitParams)
        when is_atom(CallbackModule), is_integer(Port), is_list(InitParams) ->
    start_link(CallbackModule, undefined, InitParams, [{undefined, Port, undefined}]);
    
start_link(CallbackModule, Name, Port)
        when is_atom(CallbackModule), is_integer(Port) ->
    start_link(CallbackModule, Name, [], [{undefined, Port, undefined}]);
    
start_link(CallbackModule, Port, UserData)
        when is_atom(CallbackModule), is_integer(Port) ->
    start_link(CallbackModule, undefined, [], [{undefined, Port, UserData}]);
    
start_link(CallbackModule, IpAddr, Port)
        when is_atom(CallbackModule), is_tuple(IpAddr), is_integer(Port) ->
    start_link(CallbackModule, undefined, [], [{IpAddr, Port, undefined}]).


start_link(CallbackModule, Name, Port, InitParams)
        when is_atom(CallbackModule), is_integer(Port), is_list(InitParams) ->
    start_link(CallbackModule, Name, InitParams, [{undefined, Port, undefined}]);
    
start_link(CallbackModule, IpAddr, Port, UserData)
        when is_atom(CallbackModule), is_tuple(IpAddr), is_integer(Port) ->
    start_link(CallbackModule, undefined, [], [{IpAddr, Port, UserData}]);


%%  
%% ListenerList = [{IpAddr, Port, Userdata}]
%%
start_link(CallbackModule, Name, InitParams, ListenerList)
        when is_atom(CallbackModule), is_list(InitParams), is_list(ListenerList) ->
    case Name of
        undefined           -> gen_server:start_link(?MODULE, [CallbackModule, InitParams, ListenerList], [])
    ;   X when is_atom(X)   -> gen_server:start_link({local, Name}, ?MODULE, [CallbackModule, InitParams, ListenerList], [])
    ;   _                   -> gen_server:start_link(Name, ?MODULE, [CallbackModule, InitParams, ListenerList], [])
    end.
    

%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, InitParams, Listeners]) ->
    %process_flag(trap_exit, true),

    InitState = #state{module = CallbackModule},

    try
        case CallbackModule:init(InitParams) of
            {ok, ProxyState}        ->
                start_all_listeners(Listeners, InitState#state{proxystate = ProxyState}, undefined)
        ;   {ok, ProxyState, Arg}   ->
                start_all_listeners(Listeners, InitState#state{proxystate = ProxyState}, Arg)
        ;   {stop, _} = Stop        -> Stop
        ;   ignore                  -> ignore
        ;   Err                     -> {stop, {unknown_reply, Err}}
        end
    catch
        exit:Why                    -> {stop, Why}
    end.
   
   
start_all_listeners(ListenerList, State0, Arg) ->
    StateN = lists:foldl(
                fun
                    (_Listener, {error, _} = Err)   -> Err
                    
                ;   ({IpAddr, Port, UserData}, #state{module = CallbackModule, proxystate = ProxyState} = State)   ->
                        {NIpAddr, NSockOpts} =  case IpAddr of
                                                    undefined           -> { {0,0,0,0}, ?REQUIRED_SOCK_OPTS }
                                                ;   {0,0,0,0}           -> { IpAddr, ?REQUIRED_SOCK_OPTS }
                                                ;   {_,_,_,_}           -> { IpAddr, [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS] }
                                                ;   {_,_,_,_,_,_,_,_}   -> { IpAddr, [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS] }
                                                ;   _                   -> { {0,0,0,0}, ?REQUIRED_SOCK_OPTS }
                                                end,

                        NextState = start_listener(NIpAddr, Port, UserData, NSockOpts, State),

                        case NextState of
                            {error, Reason}     ->
                                case CallbackModule:handle_error({IpAddr, Port, UserData}, {start_listener, Reason}, ProxyState) of
                                    {stop, ReasonHE, _}     -> {error, ReasonHE}
                                ;   {noreply, NewPState}    -> State#state{proxystate = NewPState}
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
    case CallbackModule:handle_call(Request, From, ProxyState) of
        {reply, Reply, NewServerState}          -> {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply, Reply, NewServerState, Arg}     -> {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply, NewServerState}               -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}          -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}          -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {stop, Reason, Reply, NewServerState}   -> {stop, Reason, Reply, State#state{proxystate = NewServerState}}
    end.


%%%%% ------------------------------------------------------- %%%%%


%handle_cast({shutdown, Reason}, State) ->
%    gen_server:terminate({shutdown, Reason}, State).
    
handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    end.
    

%%%%% ------------------------------------------------------- %%%%%


handle_info( {inet_async, ListenSock, _Ref, {ok, ClientSocket}}
           , #state{module = CallbackModule, proxystate = ProxyState, addrs = Addresses} = State) ->
    try
        {ok, {IpAddr, Port, UserData}} = dict:find(ListenSock, Addresses),
        
        case transfer_sockopt(ListenSock, ClientSocket) of
            ok                  ->
                case CallbackModule:handle_connection({IpAddr, Port, ClientSocket, UserData}) of
                    {ok, Pid}           ->
                        gen_tcp:controlling_process(ClientSocket, Pid),
                        {noreply, create_async_acceptor(ListenSock, State)}
                    
                ;   ignore              ->
                        gen_tcp:close(ClientSocket),
                        {noreply, State}
                        
                ;   {error, ReasonHC}   -> {stop, ReasonHC, State}
                end
                
        ;   {error, ReasonSO}   ->
                case CallbackModule:handle_error({IpAddr, Port, UserData}, {copy_sockopts, ReasonSO}, ProxyState) of
                    {stop, ReasonHE, NewState}  -> {stop, ReasonHE, State#state{proxystate = NewState}}
                ;   {noreply, NewState}         -> {noreply, State#state{proxystate = NewState}}
                end
                
        end

    catch exit:Why ->
        {stop, Why, State}
    end;

    
handle_info( {inet_async, ListenSock, _Ref, Error}
           , #state{module = CallbackModule, proxystate = ProxyState, addrs = Addresses} = State) ->
   
    {ok, {IpAddr, Port, UserData}} = dict:find(ListenSock, Addresses),
    
    case CallbackModule:handle_error({IpAddr, Port, UserData}, {async_accept, Error}, ProxyState) of
        {stop, ReasonHE, NewState}  -> {stop, ReasonHE, State#state{proxystate = NewState}}
    ;   {noreply, NewState}         -> {noreply, State#state{proxystate = NewState}}
    end;


%
%% Passthrough
%

handle_info(Info, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case CallbackModule:handle_info(Info, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    end.


%%%%% ------------------------------------------------------- %%%%%

trace_close(Sock) ->
    xerlang:trace({"TERMINATE", Sock}),
    gen_tcp:close(Sock).

terminate(Reason, #state{module = CallbackModule, proxystate = ProxyState, addrs = Addresses}) ->
    %[gen_tcp:close(Sock) || Sock <- dict:fetch_keys(Addresses)],
    [trace_close(Sock) || Sock <- dict:fetch_keys(Addresses)],
    CallbackModule:terminate(Reason, ProxyState),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
    

% return :: #state{} | {error, Reason}
start_listener(IpAddr, Port, UserData, TcpOpts, #state{addrs = Addresses, sockets = Sockets} = State0) ->
    case dict:find({IpAddr, Port}, Sockets) of
        {ok, _} -> State0
    ;   error   ->
            case gen_tcp:listen(Port, TcpOpts) of
                {ok, NewListenSocket} ->
                    State1 = State0#state{
                                      addrs   = dict:store(NewListenSocket, {IpAddr, Port, UserData}, Addresses)
                                    , sockets = dict:store({IpAddr, Port}, NewListenSocket, Sockets)
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

