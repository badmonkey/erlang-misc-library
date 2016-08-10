
-module(packet_processor).

-behaviour(gen_server).


%% External API
-export([start_link/3]).
 
%% gen_server callbacks 
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

     

%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        :: atom()       % callback module
    , proxystate                    :: term()       % state of callback module
    , socket        = undefined     :: inet:socket()
    , packetmode    = raw           :: packet_mode()
    , buffer        = <<>>          :: binary()
    , wait_size     = 0             :: type:natural()
    , compressor    = undefined     :: undefined | zlib:zstream()
    , udpdetails    = undefined     :: type:endpoint()
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-type fixed_size() :: 1 | 2 | 4.
-type pkt_length() :: fixed_size() | varint.
-type pkt_scheme() :: pkt_length()
                    | {chunk, N :: pos_integer()}
                    | line  % ending in nl or cr
                    | {line, EOL :: binary()}
                    | {start_tag, Tag :: binary()}.

-type simple_pkt_mode() :: raw | pkt_scheme().

-type packet_mode() :: simple_pkt_mode()
                     | udp
                     | {zlib, CompressedPkt :: pkt_scheme()}
                     | {zlib, CompressedPkt :: pkt_length(), FullSize :: pkt_length()}
                     | {zstream, Mode :: simple_pkt_mode()}.    % uncompress and then break data using Mode

-export_type([fixed_size/0, pkt_length/0, pkt_scheme/0, simple_pkt_mode/0, packet_mode/0]).


-callback init(Socket :: inet:socket(), Args :: term()) ->
      {ok, State :: term()}
    | {ok, State :: term(), timeout() | hibernate | {packet, Mode :: packet_mode()}}
    | {stop, Reason :: term()}
    | ignore.
    
    
-type data_type() :: {raw, Bytes :: binary()}
                   | {packet, Bytes :: binary()}
                   | {closed, Bytes :: binary()}
                   | {error, Reason :: term(), Bytes :: binary()}.

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

    
% gen_server callbacks (excluding init/1)    
    
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


start_link(CallbackModule, Socket, InitParams)
        when is_atom(CallbackModule), is_list(InitParams)  ->
    gen_server:start_link(?MODULE, [CallbackModule, Socket, InitParams], []).


%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, Socket, InitParams]) ->
    process_flag(trap_exit, true),

    behaviour:assert(CallbackModule, packet_processor),
    
    InitState = #state{
                      module = CallbackModule
                    , socket = Socket },

    init_callback(CallbackModule, Socket, InitParams, InitState).

    
init_callback(CallbackModule, Socket, InitParams, State) ->
    case catch CallbackModule:init(Socket, InitParams) of
        {ok, ProxyState}        -> {ok, State#state{proxystate = ProxyState}}
    ;   {ok, ProxyState, {packet, Mode}} ->
            validate_mode(Mode),
            {ok, State#state{proxystate = ProxyState, packetmode = Mode}}
    ;   {ok, ProxyState, Arg}   -> {ok, State#state{proxystate = ProxyState}, Arg}
    ;   {stop, _} = Stop        -> Stop
    ;   ignore                  -> ignore
    ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
    ;   Else                    -> {stop, {bad_return_value, Else}}
    end.
    
    
init_replace_callback(CallbackModule, Socket, InitParams, Bytes, State0) ->
    InitState = State0#state{module = CallbackModule},
    
    case init_callback(CallbackModule, Socket, InitParams, InitState) of
        {ok, StateN}    -> handle_info( {tcp, Socket, Bytes}, StateN )
    ;   {ok, StateN, _} -> handle_info( {tcp, Socket, Bytes}, StateN )
    
    ;   {stop, Reason}  -> {stop, Reason, InitState}
    ;   ignore          -> {stop, cant_ignore_replace, InitState}
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

    
handle_cast(Msg, #state{module = CallbackModule, proxystate = ProxyState} = State) ->
    case catch CallbackModule:handle_cast(Msg, ProxyState) of
        {noreply, NewServerState}       -> {noreply, State#state{proxystate = NewServerState}}
    ;   {noreply, NewServerState, Arg}  -> {noreply, State#state{proxystate = NewServerState}, Arg}
    ;   {stop, Reason, NewServerState}  -> {stop, Reason, State#state{proxystate = NewServerState}}
    ;   {'EXIT', Reason}                -> {stop, {error, Reason}, State}
    ;   Else                            -> {stop, {bad_return_value, Else}, State}
    end.
    

%%%%% ------------------------------------------------------- %%%%%


%% handle udp packets
handle_info( {udp, Socket, Ip, Port, Packet}
           , #state{packetmode = udp, module = CallbackModule, proxystate = ProxyState, socket = Socket} = State) ->
    ResetState = State#state{buffer = <<>>, wait_size = 0},
    case catch CallbackModule:handle_data({packet, Packet}, ProxyState) of
        {ok, NewState}                          ->
            {noreply, ResetState#state{proxystate = NewState}}

    ;   {reply, Reply, NewState}                ->
            _ = gen_udp:send(Socket, Ip, Port, Reply),
            {noreply, ResetState#state{proxystate = NewState}}

    ;   {stop, Reason, NewState}                ->
            {stop, Reason, ResetState#state{proxystate = NewState}}
            
    ;   {close, Reply, NewState}                ->
            _ = gen_udp:send(Socket, Ip, Port, Reply),
            {stop, normal, ResetState#state{proxystate = NewState}}
            
    ;   {replace_callback, Module, InitParams}
                when is_atom(Module), is_list(InitParams) ->
            init_replace_callback(Module, Socket, InitParams, Packet, ResetState)
            
    ;   {'EXIT', Reason}                        -> {stop, {error, Reason}, State}
    ;   Else                                    -> {stop, {bad_return_value, Else}, State}                        
    end;
    
    
handle_info( {udp, Socket, _, _, _}
           , #state{packetmode = Mode, socket = Socket} = State) ->
    {stop, {error, udp_invalid_in_this_mode, Mode}, State#state{}};
    

%% handle data but wait_size and we haven't got that much data yet
handle_info( {tcp, Socket, Data}
           , #state{buffer = Buffer, wait_size = WaitSize, socket = Socket} = State)
        when   WaitSize > 0
            , ( byte_size(Data) + byte_size(Buffer) ) < WaitSize  ->

    {noreply, State#state{buffer = <<Buffer/binary, Data/binary>>}};
    
    
%% handle data in raw mode    
handle_info( {tcp, Socket, Data}
           , #state{packetmode = raw, module = CallbackModule, proxystate = ProxyState, buffer = Buffer, socket = Socket} = State) ->
    try
        FullData = <<Buffer/binary, Data/binary>>,
        ResetState = State#state{buffer = <<>>, wait_size = 0},
        
        case catch CallbackModule:handle_data({raw, FullData}, ProxyState) of
            {ok, NewState}                          ->
                {noreply, ResetState#state{proxystate = NewState}}
                
        ;   {reply, Reply, NewState}                ->
                _ = gen_tcp:send(Socket, Reply),
                {noreply, ResetState#state{proxystate = NewState}}

        ;   {more, Length, NewState} when is_integer(Length), Length > 0 ->      
                xerlang:trace("MORE", {Length, byte_size(FullData)}),
                {noreply, ResetState#state{proxystate = NewState, buffer = FullData, wait_size = byte_size(FullData) + Length}}
                
        ;   {ok, NewState, PktType}                 ->
                validate_mode(PktType),
                {noreply, ResetState#state{proxystate = NewState, packetmode = PktType}}
                
        ;   {keep, OutBuffer, NewState, PktType} when is_binary(OutBuffer) ->
                validate_mode(PktType),
                % Process OutBuffer as if it had just arrived (OutBuffer may contain a PktType packet)
                handle_info( {tcp, Socket, OutBuffer}, ResetState#state{proxystate = NewState, packetmode = PktType} )
                
        ;   {stop, Reason, NewState}                ->
                {stop, Reason, ResetState#state{proxystate = NewState}}
                
        ;   {close, Reply, NewState}                ->
                _ = gen_tcp:send(Socket, Reply),
                {stop, normal, ResetState#state{proxystate = NewState}}
                
        ;   {replace_callback, Module, InitParams}
                    when is_atom(Module), is_list(InitParams) ->
                init_replace_callback(Module, Socket, InitParams, FullData, ResetState)
                
        ;   {replace_callback, Bytes, Module, InitParams}
                    when is_binary(Bytes), is_atom(Module), is_list(InitParams) ->
                init_replace_callback(Module, Socket, InitParams, Bytes, ResetState)
                
        ;   {'EXIT', Reason}                        -> {stop, {error, Reason}, State}
        ;   Else                                    -> {stop, {bad_return_value, Else}, State}
        end
    catch exit:Why ->
        xerlang:trace({stop, Why, State})
    end;

    
%% handle data in pkt_scheme mode    
handle_info( {tcp, Socket, Data}
           , #state{packetmode = Mode, module = CallbackModule, proxystate = ProxyState, buffer = Buffer, socket = Socket} = State) ->
    try
        FullData = <<Buffer/binary, Data/binary>>,
        ResetState = State#state{buffer = <<>>, wait_size = 0},
        
        case get_packet(Mode, FullData) of
            {ok, Value, Rest} when is_binary(Rest) ->
                case catch CallbackModule:handle_data({packet, Value}, ProxyState) of
                    {ok, NewState}                          ->
                        % check for more packets
                        handle_info( {tcp, Socket, Rest}, ResetState#state{proxystate = NewState} )

                ;   {reply, Reply, NewState}                ->
                        _ = gen_tcp:send(Socket, Reply),
                        xerlang:trace("Data Sent"),
                        % check for more packets
                        handle_info( {tcp, Socket, Rest}, ResetState#state{proxystate = NewState} )
                        
                ;   {ok, NewState, PktType}                 ->
                        validate_mode(PktType),
                        % check for more packets
                        handle_info( {tcp, Socket, Rest}, ResetState#state{proxystate = NewState, packetmode = PktType} )

                ;   {more, _, NewState}                     ->
                        {stop, {invalid_reply, {packet, Mode}}, ResetState#state{proxystate = NewState}}
                        
                ;   {keep, _, NewState, _}                  ->
                        {stop, {invalid_reply, {packet, Mode}}, ResetState#state{proxystate = NewState}}
                        
                ;   {stop, Reason, NewState}                ->
                        {stop, Reason, ResetState#state{proxystate = NewState}}
                        
                ;   {close, Reply, NewState}                ->
                        _ = gen_tcp:send(Socket, Reply),
                        {stop, normal, ResetState#state{proxystate = NewState}}
                        
                ;   {replace_callback, Module, InitParams}
                            when is_atom(Module), is_list(InitParams) ->
                        init_replace_callback(Module, Socket, InitParams, FullData, ResetState)
                
                ;   {replace_callback, Bytes, Module, InitParams}
                            when is_binary(Bytes), is_atom(Module), is_list(InitParams) ->
                        init_replace_callback(Module, Socket, InitParams, <<Bytes/binary, Rest/binary>>, ResetState)
                        
                ;   {'EXIT', Reason}                        -> {stop, {error, Reason}, State}
                ;   Else                                    -> {stop, {bad_return_value, Else}, State}                        
                end
                
        ;   {more, Length} when is_integer(Length), Length > 0 ->
                {noreply, ResetState#state{buffer = FullData, wait_size = byte_size(FullData) + Length}}
                
        ;   {error, Reason}     ->
                {stop, {error, Reason}, ResetState#state{buffer = FullData}}
        end

    catch exit:Why ->
        {stop, Why, State}
    end;
    
           
handle_info( {tcp_closed, Socket}
           , #state{module = CallbackModule, proxystate = ProxyState, buffer = Buffer, socket = Socket} = State) ->
    case catch CallbackModule:handle_data({closed, Buffer}, ProxyState) of
        {ok, NewState}              ->
            {stop, normal, State#state{proxystate = NewState}}
            
    ;   {stop, Reason, NewState}    ->
            {stop, Reason, State#state{proxystate = NewState}}
        
    ;   {'EXIT', Reason}            -> {stop, {error, Reason}, State}
    ;   Else                        -> {stop, {bad_return_value, Else}, State}
    end;


handle_info( {tcp_error, Socket, Reason}
           , #state{module = CallbackModule, proxystate = ProxyState, buffer = Buffer, socket = Socket} = State) ->
    case catch CallbackModule:handle_data({error, Reason, Buffer}, ProxyState) of
        {ok, NewState}              ->
            {stop, normal, State#state{proxystate = NewState}}
            
    ;   {stop, Reason2, NewState}    ->
            {stop, Reason2, State#state{proxystate = NewState}}
            
    ;   {reply, Reply, NewState}    ->
            _ = gen_tcp:send(Socket, Reply),
            {stop, Reason, State#state{proxystate = NewState}}

    ;   {close, Reply, NewState}    ->
            _ = gen_tcp:send(Socket, Reply),
            {stop, Reason, State#state{proxystate = NewState}}
            
    ;   {'EXIT', Reason}            -> {stop, {error, Reason}, State}
    ;   Else                        -> {stop, {bad_return_value, Else}, State}
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


validate_mode(raw) -> true;
validate_mode(1) -> true;
validate_mode(2) -> true;
validate_mode(4) -> true;
validate_mode(varint) -> true;
validate_mode({chunk,N}) when is_integer(N) -> true;
validate_mode(X) -> exit({invalid_packet_mode, X}).
    

get_packet(1, Bytes) -> bindecoder:packet_N(fun bindecoder:byte/1, Bytes);
get_packet(2, Bytes) -> bindecoder:packet_N(fun bindecoder:ushort/1, Bytes);
get_packet(4, Bytes) -> bindecoder:packet_N(fun bindecoder:ulong/1, Bytes);
get_packet(varint, Bytes) -> bindecoder:packet_N(fun bindecoder:varint/1, Bytes);
get_packet({chunk, N}, Bytes) when is_integer(N) -> bindecoder:nbytes(N, Bytes);
get_packet(Mode, _Bytes) -> {error, {invalid_mode,Mode}}.
    
    
%%%%% ------------------------------------------------------- %%%%%


terminate(Reason, #state{module = CallbackModule, proxystate = ProxyState, socket = Socket}) ->
    catch CallbackModule:terminate(Reason, ProxyState),
    catch gen_tcp:close(Socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

