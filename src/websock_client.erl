
-module(websock_client).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).


-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/1, start_link/2, start_link/3, start_link/4, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type client_mode() :: disconnected | http_connected | ws_connected.

-record(state,
    { module                            :: atom()           % callback module
    , proxystate                        :: term()           % state of callback module
    , gunner                            :: pid()
    , mref                              :: reference()
    , stream                            :: reference()
    , host                              :: string()
    , port                              :: pos_integer()
    , request                           :: string()
    , mode          = disconnected      :: client_mode()
    , json_decode   = false             :: boolean()
    , options       = []                :: [proplists:property()]
    }).

    
%%%%% ------------------------------------------------------- %%%%%    
         

-callback websock_info() -> [proplists:property()].


-callback handle_frame(Msg :: term(), State :: term()) ->
      {ok, NewState :: term()}
    | {ok, Cmd :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.         

    
% similiar to gen_server callbacks (same params but with additional return values)
% reply and noreply have new alternatives reply_send and noreply_send  which in addition
% to reply'in and noreply'in send a message to the websocket server

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
    | {reply_send, Reply :: term(), Msg :: term(), NewState :: term()}
    | {reply_send, Reply :: term(), Msg :: term(), NewState :: term(), timeout() | hibernate}    
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_cast(Request :: term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
    
-callback handle_info(Info :: timeout | term(), State :: term()) ->
      {noreply, NewState :: term()}
    | {noreply, NewState :: term(), timeout() | hibernate}
    | {noreply_send, Msg :: term(), NewState :: term()}
    | {noreply_send, Msg :: term(), NewState :: term(), timeout() | hibernate}
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
            , is_list(Opts)  ->
    start_link(undefined, CallbackModule, Url, Opts).    
    
    
start_link(Name, CallbackModule, Url, Opts)
        when  is_atom(Name)
            , is_atom(CallbackModule)
            , is_list(Url) orelse Url =:= undefined
            , is_list(Opts)  ->
    start_apps(),
    gen_server_base:start_link_name(Name, ?MODULE, [CallbackModule, Url, Opts]).    

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server

% "wss://push.planetside2.com/streaming?environment=ps2&service-id=s:example"


init([CallbackModule, Url, Opts]) ->
    process_flag(trap_exit, true),

    behaviour:assert(CallbackModule, port_server),
    
    try
        BaseOpts =  case CallbackModule:websock_info() of
                        undefined           -> undefined
                    ;   X1 when is_list(X1) -> X1
                    ;   _                   -> throw({error, invalid_wbsock_info})
                    end,
        
        SchemeDefs = http_uri:scheme_defaults() ++ [{ws, 80}, {wss, 443}],
        
        TargetUrl = case {Url, proplists:get_value(url, BaseOpts, undefined)} of
                        {undefined, undefined}  -> throw({error, no_valid_url})
                    ;   {undefined, X2}         -> X2
                    ;   {_, _}                  -> Url
                    end,
        
        case http_uri:parse(TargetUrl, [{scheme_defaults, SchemeDefs}]) of
            {error, Reason} ->
                {stop, {bad_format, Reason}}
                
        ;   {ok, {Scheme, _Userinfo, Host, Port, ParseHeaders, ParseBody} } ->
                {ok, Pid} = gun:open(Host, Port),
                Mref = monitor(process, Pid),
        
                { ok
                , #state{ gunner = Pid
                        , mref = Mref
                        , host = Host
                        , port = Port
                        , mode = disconnected
                        , request = ParseHeaders ++ ParseBody
                        , options = xproplists:merge(BaseOpts, Opts)
                        }
                }
        end
    catch exit:Why ->
        {stop, Why, #state{}}
    end.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

 
%{gun_response, Pid, StreamRef, fin, Status, Headers}
%{gun_response, Pid, StreamRef, nofin, Status, Headers}
%{gun_data, Pid, StreamRef, nofin, Data} 
%{gun_data, Pid, StreamRef, fin, Data}

% {gun_ws, Pid, close} ->
% {gun_ws, Pid, {close, Code, _}} ->
% {gun_ws, Pid, Frame} ->
% {gun_down, Pid, ws, _, _, _} ->
% {gun_ws, Pid, {text, Text}} ->
% {gun_ws, Pid, Frame}


handle_info( {gun_up, Pid, _Protocol}, #state{ gunner = Pid, request = Request } = State) ->
    lager:debug("gunner up, upgrade to websock"),
    Streamref = gun:ws_upgrade(Pid, Request),
    {noreply, State#state{ stream = Streamref, mode = http_connected } };
    
    
handle_info( {'DOWN', Mref, process, Pid, _Reason}, #state{ gunner = Pid, mref = Mref } = State) ->
    lager:debug("gunner process down"),
    {noreply, State#state{ mode = disconnected }};
    
    
handle_info( {gun_ws_upgrade, Pid, ok, _Headers}, #state{ gunner = Pid } = State)->
    lager:debug("gunner is now webscale"),
    {noreply, State#state{ mode = ws_connected }};
    
    

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


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


