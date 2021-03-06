
-module(websock_client).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).


-include_lib("highgarden/include/supervisors.hrl").


-export([start_link/1, start_link/2, start_link/3, start_link/4, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
         
-define(WEBSOCK_TAG, 'websock$tag').
         


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type client_mode() :: disconnected | http_pending | http_connected | ws_connected.

-record(state,
    { module                            :: atom()           % callback module
    , proxystate                        :: term()           % state of callback module
    , gunner        = undefined         :: undefined | pid()
    , mref          = undefined         :: undefined | reference()
    , stream                            :: reference()
    , host                              :: string()
    , port                              :: pos_integer()
    , request                           :: string()
    , mode          = disconnected      :: client_mode()
    , frame_handler = json              :: text | json
    }).

    
%%%%% ------------------------------------------------------- %%%%%    
         

-callback websock_info( type:properties() ) -> type:properties().


-callback handle_frame(Msg :: term(), State :: term()) ->
      {ok, NewState :: term()}
    | {ok, Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.         

    
% similiar to gen_server callbacks (same params; except init, but with additional return values)
% reply and noreply have new alternatives reply_send and noreply_send  which in addition
% to reply'in and noreply'in send a message to the websocket server

-callback init(Url :: string(), Args :: type:properties()) ->
      {ok, State :: term()}
    | {ok, State :: term(), timeout() | hibernate}
    | {stop, Reason :: term()} | ignore.
    
-callback handle_call( Request :: term()
                     , From :: {pid(), Tag :: term()}
                     , State :: term()) ->
      {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), timeout() | hibernate}
    | {reply_send, Reply :: term(), Msg :: term(), NewState :: term()}
    | {reply_send, Reply :: term(), Msg :: term(), NewState :: term(), timeout() | hibernate}    
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
    
%-callback handle_call(_, gen_type:from(), State) -> gen_type:call_result(State)
%                                                  | gen_type:send_result(State) when State :: term().
    
-callback handle_cast(Request :: term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
    
%-callback handle_cast(_, State) -> gen_type:cast_result(State) | gen_type:noreply_send(State) when State :: term(). 
    
-callback handle_info(Info :: timeout | term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
    | {connect, NewState :: term()}
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
% Public API


start_apps() ->
    application:ensure_all_started(gun),
    application:ensure_all_started(lager).
    
    
start_link(CallbackModule) ->
    start_link(undefined, CallbackModule, undefined, []).
    
    
start_link(Name, CallbackModule)
        when  is_atom(Name)
            , is_atom(CallbackModule)  ->
    start_link(Name, CallbackModule, undefined, []);
    
start_link(CallbackModule, Url)
        when  is_atom(CallbackModule)
            , is_list(Url)  ->
    start_link(undefined, CallbackModule, Url, []).

    
start_link(Name, CallbackModule, Url)
        when  is_atom(Name)
            , is_atom(CallbackModule)
            , is_list(Url)  ->
    start_link(Name, CallbackModule, Url, []);
    
start_link(CallbackModule, Url, Opts)
        when  is_atom(CallbackModule)
            , is_list(Url)
            , is_list(Opts) orelse is_map(Opts)  ->
    start_link(undefined, CallbackModule, Url, Opts).    
    
    
start_link(Name, CallbackModule, Url, Opts)
        when  is_atom(Name)
            , is_atom(CallbackModule)
            , is_list(Url) orelse Url =:= undefined
            , is_list(Opts) orelse is_map(Opts)  ->
    start_apps(),
    gen_server_base:start_link_name(Name, ?MODULE, [CallbackModule, Url, Opts]).    

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init([CallbackModule, Url, Opts]) ->
    process_flag(trap_exit, true),

    behaviour:assert(CallbackModule, websock_client),
    
    lager:info("Starting websock_client with module = ~p, url = ~p, opts = ~p", [CallbackModule, Url, Opts]),
    
    try
        BaseOpts =  case CallbackModule:websock_info(Opts) of
                        undefined           -> undefined
                    ;   X1 when is_list(X1) -> X1
                    ;   X1 when is_map(X1)  -> X1
                    ;   _                   -> throw({error, invalid_wbsock_info})
                    end,
        
        SchemeDefs = http_uri:scheme_defaults() ++ [{ws, 80}, {wss, 443}],
        
        TargetUrl = case {Url, property:get_value(url, BaseOpts, undefined)} of
                        {undefined, undefined}  -> throw({error, no_valid_url})
                    ;   {undefined, X2}         -> X2
                    ;   {_, _}                  -> Url
                    end,
        
        case http_uri:parse(TargetUrl, [{scheme_defaults, SchemeDefs}]) of
            {error, Reason} ->
                {stop, {bad_format, Reason}}
                
        ;   {ok, {Scheme, _Userinfo, Host, Port, ParseHeaders, ParseBody} } ->
                lager:info("websock_client begin with  Url = ~p, Host = ~p:~p", [TargetUrl, Host, Port]),
                
                InitOpts = property:merge(BaseOpts, Opts),

                InitState = #state{ module = CallbackModule
                                  , host = Host
                                  , port = Port
                                  , mode = disconnected
                                  , request = ParseHeaders ++ ParseBody
                                  },
                                  
                OpenFun =   fun(#state{} = State) ->
                                case property:get_bool(defer_startup, InitOpts) of
                                    true    -> State
                                ;   false   -> open_gun(State)
                                end
                            end,
                
                case catch CallbackModule:init(TargetUrl, InitOpts) of
                    {ok, ProxyState}        -> {ok, OpenFun(InitState#state{proxystate = ProxyState})}
                ;   {ok, ProxyState, Arg}   -> {ok, OpenFun(InitState#state{proxystate = ProxyState}), Arg}
                ;   {stop, _} = Stop        -> Stop
                ;   ignore                  -> ignore
                ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
                ;   Else                    -> {stop, {bad_return_value, Else}}
                end

        end
    catch exit:Why ->
        {stop, Why, #state{}}
    end.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_call(Request, From, ProxyState) of
        {reply, Reply, NewServerState}                  -> {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply, Reply, NewServerState, Arg}             -> {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {reply_send, Msg, Reply, NewServerState}        -> send_data(Msg, State), {reply, Reply, State#state{proxystate = NewServerState}}
    ;   {reply_send, Msg, Reply, NewServerState, Arg}   -> send_data(Msg, State), {reply, Reply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply, NewServerState}                       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}                  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Msg, NewServerState}             -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Msg, NewServerState, Arg}        -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}, Arg}
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
    ;   {noreply_send, Msg, NewServerState}         -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Msg, NewServerState, Arg}    -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                            -> {stop, {error, Reason}, State}
    ;   Else                                        -> {stop, {bad_return_value, Else}, State}
    end.

    
%%%%% ------------------------------------------------------- %%%%%

        
handle_info( {gun_up, Pid, Protocol}, #state{ gunner = Pid, request = Request } = State) ->
    lager:debug("Gunner up, upgrade to websock ~p", [Protocol]),
    Streamref = gun:ws_upgrade(Pid, Request),
    {noreply, State#state{ stream = Streamref, mode = http_connected } };
    
    
handle_info( {gun_down, Pid, ws, _, _, _}, #state{ gunner = Pid } = State) ->
    lager:debug("Gunner process gun_down"),
    NewState = close_gun(State),
    {noreply, open_gun(NewState)};
    
    
handle_info( {'DOWN', Mref, process, Pid, Reason}, #state{ gunner = Pid, mref = Mref } = State) ->
    lager:debug("Gunner process Down ~p", [Reason]),
    NewState = close_gun(State),
    {noreply, open_gun(NewState)};
    
    
handle_info( {gun_ws_upgrade, Pid, ok, _Headers}, #state{ gunner = Pid } = State) ->
    lager:info("Websock_client is now ready {~p, ~p}", [State#state.host, State#state.request]),
    handle_frame_callback({system, up}, State#state{ mode = ws_connected });
    
    
handle_info( {gun_ws, Pid, close}, #state{ gunner = Pid } = State) ->
    gun:ws_send(Pid, close),
    {noreply, State};


handle_info( {gun_ws, Pid, {close, Code, _}}, #state{ gunner = Pid } = State) ->
    gun:ws_send(Pid, {close, Code, <<>>}),
    {noreply, State};
    

handle_info( {gun_ws, Pid, {system, _} = Data}, #state{ gunner = Pid } = State) ->    
    lager:info("Websock_client system change: ~p}", [Data]),
    handle_frame_callback(Data, State);

    
handle_info( {gun_ws, Pid, {text, Data}}, #state{ gunner = Pid, frame_handler = json } = State) ->
    % TODO better error handling of decode
    handle_frame_callback({json, jiffy:decode(Data, [return_maps])}, State);
    

handle_info( {gun_ws, Pid, {text, Data}}, #state{ gunner = Pid, frame_handler = text } = State) ->    
    handle_frame_callback({text, Data}, State);    

% {gun_ws, Pid, Frame}


%
%% Passthrough
%

handle_info(Info, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    lager:debug("Passthrough ~p", [Info]),
    
    case catch CallbackModule:handle_info(Info, ProxyState) of
        {noreply, NewServerState}                   -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}              -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {noreply_send, Msg, NewServerState}         -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply_send, Msg, NewServerState, Arg}    -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {connect, NewServerState}                   -> {noreply, open_gun(State#state{proxystate = NewServerState})}
    ;   {stop, Reason, NewServerState}              -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                            -> {stop, {error, Reason}, State}
    ;   Else                                        -> {stop, {bad_return_value, Else}, State}
    end.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


send_data(undefined, #state{}) ->
    ok;

send_data(_, #state{ mode = disconnected }) ->   
    ok;

    
send_data([], #state{}) ->
    ok;
    
send_data([Hd | Rest], #state{} = State) ->
    send_data(Hd, State),
    send_data(Rest, State);
    

send_data(close, #state{ gunner = Pid }) ->
    gun:ws_send(Pid, close);
    

send_data({json, Data}, #state{ gunner = Pid, frame_handler = json }) ->
    gun:ws_send(Pid, {text, Data});
    
send_data({text, Data}, #state{ gunner = Pid, frame_handler = json }) ->
    gun:ws_send(Pid, {text, jiffy:encode(Data)});
    
send_data({text, Data}, #state{ gunner = Pid, frame_handler = text }) ->
    gun:ws_send(Pid, {text, Data});
    
    
send_data(Data, #state{ gunner = Pid, frame_handler = json }) ->
    gun:ws_send(Pid, {text, jiffy:encode(Data)});
    
send_data(Data, #state{ gunner = Pid, frame_handler = text }) ->
    gun:ws_send(Pid, {text, Data}).


%%%%% ------------------------------------------------------- %%%%%
    
    
handle_frame_callback(Data, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_frame(Data, ProxyState) of
        {ok, NewServerState}            -> {noreply, State#state{proxystate = NewServerState}}
    ;   {ok, Msg, NewServerState}       -> send_data(Msg, State), {noreply, State#state{proxystate = NewServerState}}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                -> {stop, {error, Reason}, State}
    ;   Else                            -> {stop, {bad_return_value, Else}, State}
    end.
    


close_gun(#state{ mode = disconnected } = State) -> State;

close_gun(#state{ gunner = Pid, mref = Ref } = State) ->
    demonitor(Ref),
    gun:close(Pid),
    gun:flush(Pid),
    State#state{ mode = disconnected, gunner = undefined, mref = undefined }.
    
    
open_gun(#state{ mode = disconnected, host = Host, port = Port } = State) ->
    lager:debug("Attempting to reconnect ~p:~p", [Host, Port]),
    {ok, Pid} = gun:open(Host, Port),
    Mref = monitor(process, Pid),
    State#state{ gunner = Pid, mref = Mref, mode = http_pending };
    
open_gun(#state{} = State) -> State.
    

