
-module(udp_server).

-behaviour(gen_server).


%% External API
-export([start_link/2]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).


-define(COPY_SOCK_OPTS, [active, nodelay, keepalive, delay_send, priority, tos, sndbuf]).
-define(REQUIRED_SOCK_OPTS, [binary, {packet, raw}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}]).

     
%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        % callback module
    , handlers
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-callback handle_connection(Connection :: connection_type(), UserData :: term(), State :: term()) ->
      type:start_result()
    | {ok, Pid :: pid(), State1 :: term()}
    | {reject, State1 :: term()}
    | {stop, Reason :: term(), State1 :: term()}.
    
-callback handle_error(Info :: listener_type() | connection_type(), Reason :: term(), State :: term()) ->
      {stop, Reason1 :: term(), State1 :: term()}
    | {noreply, State1 :: term()}.

-callback handle_data(Input :: data_type(), State :: term()) ->
      {ok, State1 :: term()}
    | {reply, Bytes :: binary(), State1 :: term()}
    | {more, Length :: pos_integer(), State1 :: term()}
    | {ok, State1 :: term(), PktType :: packet_mode()}
    | {keep, Buffer :: binary(), State1 :: term(), PktType :: packet_mode()} 
    | {stop, Reason :: term(), State1 :: term()}
    | {close, Reply :: binary(), State1 :: term()}
    | {replace_callback, Module :: atom(), InitParams :: term()}
    | {replace_callback, Bytes :: binary(), Module :: atom(), InitParams :: term()}.
    
    
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


% handle_info({udp, Socket, IP, InPortNo, Packet})

% handle_call({udp$server, what, Data, Socket, IP, InPortNo, Packet})







-module(SomethingServer).


start_link(Port, Period = 5min) ->
	udp_server:start_link(?MODULE, Port, Period, []).
	

start_client(Socket) ->
init() ->
client_init() ->


% listener	
handle_new_client({Addr, Port}, State) ->
	

handle_client_heartbeat(blocked, {Addr, Port}, State) -> %listener
handle_client_heartbeat(??, {Addr, Port}, State) -> %client

%client
handle_packet(Socket, {Addr, Port}, Data) ->



