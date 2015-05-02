
-module(udp_server).

-behaviour(gen_server).


%% External API
-export([start_link/2, start_link/3, start_link/6]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

        
     
%%%%% ------------------------------------------------------- %%%%%


-record(client_handler,
    { pid                               :: pid()
    , monitorref                        :: reference()
    }).
    

-record(state, 
    { module                            :: atom()           % callback module
    , proxystate                        :: term()           % state of callback module
    , heartbeatMS                       :: pos_integer()    % in milliseconds
    , timer         = nil               :: timer:tref()
    , socket        = undefined         :: inet:socket()
    , handlers      = gb_tree:empty()   :: gb_tree:tree( type:endpoint(), rejected | #client_handler{} )
    , references    = gb_tree:empty()   :: gb_tree:tree( reference(), type:endpoint() )
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-callback handle_new_client(Endpoint :: type:endpoint(), Socket :: inet:socket(), State :: term()) ->
      type:start_result()
    | {ok, Pid :: pid(), State1 :: term()}
    | {reject, State1 :: term()}
    | {stop, Reason :: term(), State1 :: term()}.
    
-callback handle_heartbeat(rejected, Endpoint :: type:endpoint(), State :: term()) ->
      {clear, State1 :: term()}
    | {reject, State1 :: term()}
    | {stop, Reason :: term(), State1 :: term()}.
    
    
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


-define(DEFAULT_HEARTBEAT, 5*60).


start_link(CallbackModule, Port)
        when is_atom(CallbackModule), is_integer(Port)  ->
    start_link(CallbackModule, undefined, undefined, Port, [], ?DEFAULT_HEARTBEAT).

start_link(CallbackModule, Port, InitParams)
        when is_atom(CallbackModule), is_integer(Port), is_list(InitParams)  ->
    start_link(CallbackModule, undefined, undefined, Port, InitParams, ?DEFAULT_HEARTBEAT).
    
    
start_link(CallbackModule, Name, IpAddr, Port, InitParams, HeartbeatSecs)
        when  is_atom(CallbackModule)
            , is_tuple(IpAddr), is_integer(Port)
            , is_list(InitParams), is_integer(HeartbeatSecs)  ->
    case Name of
        undefined           -> gen_server:start_link(?MODULE, [CallbackModule, InitParams, {IpAddr, Port}, HeartbeatSecs], [])
    ;   X when is_atom(X)   -> gen_server:start_link({local, Name}, ?MODULE, [CallbackModule, InitParams, {IpAddr, Port}, HeartbeatSecs], [])
    ;   _                   -> gen_server:start_link(Name, ?MODULE, [CallbackModule, InitParams, {IpAddr, Port}, HeartbeatSecs], [])
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, InitParams, Endpoint, HeartbeatSecs]) ->
    process_flag(trap_exit, true),
    
    behaviour:assert(CallbackModule, udp_server),

    InitState = #state{module = CallbackModule, heartbeatMS = HeartbeatSecs * 1000},

    case catch CallbackModule:init(InitParams) of
        {ok, ProxyState}        -> {ok, start_listener(Endpoint, InitState#state{ proxystate = ProxyState })}
    ;   {ok, ProxyState, Arg}   -> {ok, start_listener(Endpoint, InitState#state{ proxystate = ProxyState }), Arg}
    ;   {stop, _} = Stop        -> Stop
    ;   ignore                  -> ignore
    ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
    ;   Else                    -> {stop, {bad_return_value, Else}}
    end.
    
    
-define(REQUIRED_SOCK_OPTS, [binary, active]).    
    
start_listener({IpAddr, Port}, #state{heartbeatMS = HeartBeat} = State) ->
    SockOpts =  case IpAddr of
                    {_,_,_,_}           -> [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS]
                ;   {_,_,_,_,_,_,_,_}   -> [{ip, IpAddr} | ?REQUIRED_SOCK_OPTS]
                ;   _                   -> ?REQUIRED_SOCK_OPTS
                end,
    {ok, Socket} = gen_udp:open(Port, SockOpts),
    {ok, TRef} = timer:send_interval(HeartBeat, {'udp$server', heartbeat}),
    State#state{ socket = Socket, timer = TRef }.
    
    
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


handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                -> {stop, {error, Reason}, State}
    ;   Else                            -> {stop, {bad_return_value, Else}, State}
    end.
    

%%%%% ------------------------------------------------------- %%%%%    


handle_info( {udp, Socket, Ip, Port, _Packet} = UdpPacket
           , #state{module = CallbackModule, proxystate = ProxyState, handlers = Handlers, references = References} = State) ->
    Endpoint = {Ip, Port},
    case gb_trees:lookup(Endpoint, Handlers) of
        none ->
            case catch CallbackModule:handle_new_client(Endpoint, Socket, ProxyState) of
                {ok, Pid}           ->
                    MRef = erlang:monitor(process, Pid),
                    NewHandlers = gb_trees:insert(Endpoint, #client_handler{pid = Pid, monitorref = MRef}, Handlers),
                    NewReferences = gb_tree:insert(MRef, Endpoint, References),
                    Pid ! UdpPacket,
                    {noreply, State#state{handlers = NewHandlers, references = NewReferences}}
                
            ;   ignore              ->
                    NewHandlers = gb_trees:insert(Endpoint, rejected, Handlers),
                    {noreply, State#state{handlers = NewHandlers}}
                    
            ;   {error, Reason}   ->
                    {stop, {error, Reason}, State}
            
            ;   {ok, Pid, NewState} ->
                    MRef = erlang:monitor(process, Pid),
                    NewHandlers = gb_trees:insert(Endpoint, #client_handler{pid = Pid, monitorref = MRef}, Handlers),
                    NewReferences = gb_tree:insert(MRef, Endpoint, References),
                    Pid ! UdpPacket,
                    {noreply, State#state{proxystate = NewState, handlers = NewHandlers, references = NewReferences}}
                    
            ;   {ignore, NewState}  ->
                    NewHandlers = gb_trees:insert(Endpoint, rejected, Handlers),
                    {noreply, State#state{proxystate = NewState, handlers = NewHandlers}}
            
            ;   {stop, Reason, NewState}  ->
                    {stop, Reason, State#state{proxystate = NewState}}
            
            ;   {'EXIT', Reason}    ->
                    {stop, {error, Reason}, State}
                    
            ;   Else                ->
                    {stop, {bad_return_value, Else}, State}
            end
                                        
    ;   {value, ClientHandler} ->
            case ClientHandler of
                rejected                    ->
                    {noreply, State}
                    
            ;   #client_handler{pid = Pid}  ->
                    Pid ! UdpPacket,
                    {noreply, State}
            end
    end;
    
            
handle_info({'DOWN', MonitorRef, process, Object, Info}, State) ->
    {noreply, State};
    
    
handle_info( {'udp$server', heartbeat}
           , #state{module = CallbackModule, proxystate = ProxyState, handlers = Handlers} = State) ->
    
    Proc =  fun
                (_, {{stop, _} = Err, AccState}) ->
                    {true, {Err, AccState}}
                    
            ;   ({Endpoint, rejected}, {_, AccState}) ->
                    case process_handler(rejected, Endpoint, AccState) of
                        {delete, NewState}          -> {false, {ok, NewState}}
                    ;   {keep, NewState}            -> {true, {ok, NewState}}
                    ;   {stop, Reason, NewState}    -> {true, {{stop, Reason}, NewState}}
                    end
                    
            ;   (_, Acc)    -> {true, Acc}
            end,
            
    {NewTree, Result} = xlists:filter_fold(Proc, {ok, State}, gb_tree:to_list(Handlers) ),
    NewHandlers = gb_tree:from_orddict(NewTree),
    
    case Result of
        {ok, AState} ->
            {noreply, AState#state{ handlers = NewHandlers }}
            
    ;   {{stop, Reason}, AState} ->
            {stop, Reason, AState#state{ handlers = NewHandlers }}
    end;


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


terminate(Reason, #state{module = CallbackModule, proxystate = ProxyState, socket = Socket}) ->
    gen_udp:close(Socket),
    catch CallbackModule:terminate(Reason, ProxyState).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%


-spec process_handler(rejected, type:endpoint(), #state{}) -> {keep, #state{}} | {delete, #state{}} | {stop, term(), #state{}}.

process_handler( rejected, Endpoint
               , #state{module = CallbackModule, proxystate = ProxyState} = State)  ->
               
    case catch CallbackModule:handle_heartbeat(rejected, Endpoint, ProxyState) of
        {clear, NewState}  ->
            {delete, State#state{proxystate = NewState}}
            
    ;   {reject, NewState}  ->
            {keep, State#state{proxystate = NewState}}

    ;   {'EXIT', Reason}    ->
            {stop, {error, Reason}, State}
            
    ;   Else                ->
            {stop, {bad_return_value, Else}, State}     
    end;

    
process_handler(_, _, State) ->
    {keep, State}.

