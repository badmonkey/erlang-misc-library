
-module(port_server).

-behaviour(gen_server).


%% External API
-export([start_link/3, start_link/4]).
 
%% gen_server callbacks 
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

     

%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        :: atom()       % callback module
    , proxystate                    :: term()       % state of callback module
    , port
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-callback handle_port(Msg :: term(), State :: term()) ->
      {ok, NewState :: term()}
    | {ok, Cmd :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.

    
% similiar to gen_server callbacks (same params but with additional return values)   

-callback init(Args :: term()) ->
      {ok, State :: term()}
    | {ok, State :: term(), timeout()
    | hibernate}
    | {stop, Reason :: term()} | ignore.
    
-callback handle_call( Request :: term()
                     , From :: {pid(), Tag :: term()}
                     , State :: term()) ->
      {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), timeout() | hibernate}
    | {reply_send, Reply :: term(), Cmd :: term(), NewState :: term()}
    | {reply_send, Reply :: term(), Cmd :: term(), NewState :: term(), timeout() | hibernate}    
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Cmd :: term(), NewState :: term()}
    | {noreply_send, Cmd :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_cast(Request :: term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Cmd :: term(), NewState :: term()}
    | {noreply_send, Cmd :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_info(Info :: timeout | term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Cmd :: term(), NewState :: term()}
    | {noreply_send, Cmd :: term(), NewState :: term(), timeout() | hibernate}
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


start_link(CallbackModule, ExeFile, InitParams)
        when is_atom(CallbackModule), is_list(ExeFile), is_list(InitParams)  ->
    gen_server:start_link(?MODULE, [CallbackModule, ExeFile, InitParams], []).
    
    
start_link(Name, CallbackModule, ExeFile, InitParams)
        when is_atom(Name), is_atom(CallbackModule), is_list(ExeFile), is_list(InitParams)  ->
    gen_server:start_link({local, Name}, ?MODULE, [CallbackModule, ExeFile, InitParams], []).    
    

%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, ExeFile, InitParams]) ->
    process_flag(trap_exit, true),

    Port = erlang:open_port({spawn, ExeFile}, [{packet, 2}, binary, exit_status]),

    InitState = #state{module = CallbackModule, port = Port},

    try
        case CallbackModule:init(InitParams) of
            {ok, ProxyState}        -> {ok, InitState#state{proxystate = ProxyState}}
        ;   {ok, ProxyState, Arg}   -> {ok, InitState#state{proxystate = ProxyState}, Arg}
        ;   {stop, _} = Stop        -> Stop
        ;   ignore                  -> ignore
        ;   Err                     -> {stop, {unknown_reply, Err}}
        end
    catch
        exit:Why                    -> {stop, Why}
    end.



%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case CallbackModule:handle_call(Request, From, ProxyState) of
        {reply, Reply, NewServerState}                  -> {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply, Reply, NewServerState, Arg}             -> {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {reply_send, Reply, Cmd, NewServerState}        -> port_send(Cmd, State), {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply_send, Reply, Cmd, NewServerState, Arg}   -> port_send(Cmd, State), {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply, NewServerState}                       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}                  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Cmd, NewServerState}             -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Cmd, NewServerState, Arg}        -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}                  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {stop, Reason, Reply, NewServerState}           -> {stop, Reason, Reply, State#state{proxystate = NewServerState}}
    end.


%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}                   -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}              -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Cmd, NewServerState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Cmd, NewServerState, Arg}    -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    end.
    

%%%%% ------------------------------------------------------- %%%%%


handle_info( {Port, {data, Data}}
           , #state{module = CallbackModule, proxystate = ProxyState, port = Port} = State) ->
    try
        case CallbackModule:handle_port(binary_to_term(Data), ProxyState) of
            {ok, NewState}              -> {noreply, State#state{proxystate = NewState}}
        ;   {ok, Cmd, NewState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewState}}
        ;   {stop, Reason, NewState}    -> {stop, Reason, State#state{proxystate = NewState}}
        end
    catch exit:Why ->
        {stop, Why, State}
    end;
    
      
handle_info( {Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, {port_terminated, Status}, State};


handle_info( {'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, Reason, State};


%
%% Passthrough
%

handle_info(Info, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case CallbackModule:handle_info(Info, ProxyState) of
        {noreply, NewServerState}                   -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}              -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Cmd, NewServerState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Cmd, NewServerState, Arg}    -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    end.


%%%%% ------------------------------------------------------- %%%%%


terminate(Reason, #state{module = CallbackModule, proxystate = ProxyState, port = Port}) ->
    catch CallbackModule:terminate(Reason, ProxyState),
    erlang:close_port(Port),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%% ------------------------------------------------------- %%%%%


port_send(Msg, #state{port = Port}) ->
    erlang:port_command(Port, term_to_binary(Msg)).
    