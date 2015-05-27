
-module(port_server).
-extends(gen_server_base).

-behaviour(gen_server).


%% External API
-export([start_link/2, start_link/3]).
 
%% gen_server callbacks 
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

     

%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        :: atom()       % callback module
    , proxystate                    :: term()       % state of callback module
    , port
    , btt_opts                      :: [atom()]
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-callback port_info() -> [proplists:property()].


-callback handle_port(Msg :: term(), State :: term()) ->
      {ok, NewState :: term()}
    | {ok, Cmd :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.

    
% similiar to gen_server callbacks (same params but with additional return values)
% reply and noreply have new alternatives reply_send and noreply_send  which in addition
% to reply'in and noreply'in send a cmd to the port

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


start_link(CallbackModule, InitParams)
        when is_atom(CallbackModule), is_list(InitParams)  ->
    gen_server:start_link(?MODULE, [CallbackModule, InitParams], []).
    
    
start_link(Name, CallbackModule, InitParams)
        when is_atom(Name), is_atom(CallbackModule), is_list(InitParams)  ->
    gen_server:start_link({local, Name}, ?MODULE, [CallbackModule, InitParams], []).    
    

%%%%% ------------------------------------------------------- %%%%%

%
% {system, "some_cmd --params"}         -- [stream]
% {driver, {"command", Args}}           -- [{packet, 2}, {args, Args}]
% {driver, {appname, "command", Args}}  -- [{packet, 2}, {args, Args}]
% {packet, 1 | 2 | 4 | raw}
% safe
%


init([CallbackModule, InitParams]) ->
    process_flag(trap_exit, true),

    behaviour:assert(CallbackModule, port_server),
    
    case catch CallbackModule:port_info() of 
        {'EXIT', Reason}    -> {stop, {error, Reason}}
        
    ;   X when is_list(X)   ->
            {StartCmd, BaseOpts} = 
                case proplists:get_value(system, X, undefined) of
                    undefined   ->
                        case proplists:get_value(driver, X, undefined) of
                            undefined                   ->
                                { undefined, [] }
                            
                        ;   {Cmd, Args}
                                    when is_list(Args)  ->
                                { {spawn_executable, xos:find_executable(Cmd)}, [{packet, 2}, {args, Args}] }
                        
                        ;   {App, Cmd, Args}
                                    when  is_atom(App)
                                        , is_list(Args) ->
                                { {spawn_executable, xos:find_executable(App, Cmd)}, [{packet, 2}, {args, Args}] }
                        end
                        
                ;   Cmd         -> { {spawn, Cmd}, [stream] }
                end,
                
            Opts =  case proplists:get_value(packet, X, undefined) of
                        undefined   -> BaseOpts
                    ;   raw         -> xproplists:delete_append([packet], [stream], BaseOpts)
                    ;   N when  N =:= 1 orelse N =:= 2 orelse N =:= 4  ->
                            xproplists:delete_append([stream], [{packet, N}], BaseOpts)
                    end,

            BTT_opts =  case proplists:get_value(safe, X, undefined) of
                            undefined   -> []
                        ;   true        -> [safe]
                        end,
            
            Port = erlang:open_port(StartCmd, xproplists:merge(Opts, [binary, exit_status])),

            InitState = #state{module = CallbackModule, port = Port, btt_opts = BTT_opts},

            case catch CallbackModule:init(InitParams) of
                {ok, ProxyState}        -> {ok, InitState#state{proxystate = ProxyState}}
            ;   {ok, ProxyState, Arg}   -> {ok, InitState#state{proxystate = ProxyState}, Arg}
            ;   {stop, _} = Stop        -> Stop
            ;   ignore                  -> ignore
            ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
            ;   Else                    -> {stop, {bad_return_value, Else}}
            end
            
    ;   Else                ->
            {stop, {bad_return_value, Else}}
    end.


%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_call(Request, From, ProxyState) of
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
    ;   {'EXIT', Reason}                                -> {stop, {error, Reason}, State}
    ;   Else                                            -> {stop, {bad_return_value, Else}, State}
    end.


%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}                   -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}              -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Cmd, NewServerState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Cmd, NewServerState, Arg}    -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                            -> {stop, {error, Reason}, State}
    ;   Else                                        -> {stop, {bad_return_value, Else}, State}
    end.


%%%%% ------------------------------------------------------- %%%%%


handle_info( {Port, {data, Data}}
           , #state{module = CallbackModule, proxystate = ProxyState, port = Port, btt_opts = BTT_opts} = State) ->
    case catch CallbackModule:handle_port(binary_to_term(Data, BTT_opts), ProxyState) of
        {ok, NewState}              -> {noreply, State#state{proxystate = NewState}}
    ;   {ok, Cmd, NewState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewState}}
    ;   {stop, Reason, NewState}    -> {stop, Reason, State#state{proxystate = NewState}}
    ;   {'EXIT', Reason}            -> {stop, {error, Reason}, State}
    ;   Else                        -> {stop, {bad_return_value, Else}, State}
    end;
    
      
handle_info( {Port, {exit_status, Status}}, #state{port = Port} = State) ->
    % handle_exit() -> terminate | restart
    {stop, {port_terminated, Status}, State};


handle_info( {'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, Reason, State};


%
%% Passthrough
%

handle_info(Info, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_info(Info, ProxyState) of
        {noreply, NewServerState}                   -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}              -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Cmd, NewServerState}         -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Cmd, NewServerState, Arg}    -> port_send(Cmd, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                            -> {stop, {error, Reason}, State}
    ;   Else                                        -> {stop, {bad_return_value, Else}, State}
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
    