
-module(table_service).

-behaviour(gen_server).


%% External API
-export([start_link/1, start_link/2, start_link/3]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

        
-define(TABLE_SERVICE_TAG, 'table$service').


% TODO upgrading tables via mnesia:transform_table(Tab, Fun, NewAttributeList, NewRecordName) 
     
     
%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { module                        :: atom()       % callback module
    , proxystate                    :: term()       % state of callback module
    , tables        = []            :: [atom()]
    }).

    
%%%%% ------------------------------------------------------- %%%%%


-callback tables() ->       % type:atomlist().
    [atom()].
    
-callback table_info(Table :: atom()) ->    % type:properties().
    [proplists:property()].

    
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
% Public API


start_link(CallbackModule) ->
    start_link(CallbackModule, undefined, []).
    
    
start_link(CallbackModule, InitParams) ->
    start_link(CallbackModule, undefined, InitParams).    
    

start_link(CallbackModule, Name, InitParams)
        when  is_atom(CallbackModule)
            , is_atom(Name) orelse is_tuple(Name)
            , is_list(InitParams)  ->
    gen_server_base:start_link_name(Name, ?MODULE, [CallbackModule, InitParams]).


%%%%% ------------------------------------------------------- %%%%%


init([CallbackModule, InitParams]) ->
    process_flag(trap_exit, true),
    
    behaviour:assert(CallbackModule, table_service),

    case catch CallbackModule:tables() of 
        {'EXIT', Reason}    -> {stop, {error, Reason}}
        
    ;   Tables when is_list(Tables) ->
            InitState = #state{module = CallbackModule, tables = Tables},
            case create_tables(Tables, InitState) of
                {ok, StateN}    ->
                    case catch CallbackModule:init(InitParams) of
                        {ok, ProxyState}        -> {ok, StateN#state{proxystate = ProxyState}}
                    ;   {ok, ProxyState, Arg}   -> {ok, StateN#state{proxystate = ProxyState}, Arg}
                    ;   {stop, _} = Stop        -> Stop
                    ;   ignore                  -> ignore
                    ;   {'EXIT', Reason}        -> {stop, {error, Reason}}
                    ;   Else                    -> {stop, {bad_return_value, Else}}
                    end
                    
            ;   {stop, Error}   -> {stop, Error}
            end
            
    ;   Else                ->
            {stop, {bad_return_value, Else}}
    end.
    
    
create_tables(Tables, #state{} = State) ->    
    CurrentTables = mnesia:system_info(tables),
    
    lager:info("Required tables ~p", [Tables]),
    
        % if there are no tables yet, go ahead and create the schema on disk
        % TODO should only do this if the node is not discless
        % TODO global lock? Many table_service might want to create teh schema
    case CurrentTables of
        [schema]    ->
            mnesia:stop(),
            lager:debug("creating schema"),
            catch mnesia:create_schema([node()]), 
            mnesia:start()
    ;   _Else       -> ok
    end,
    
    case create_tables(Tables, [], CurrentTables, State) of
        {ok, Created, StateN}   ->
            lager:debug("Waiting for tables ~p", [Created]),
            case mnesia:wait_for_tables(Created, infinity) of
                ok                  ->
                    {ok, StateN}
                    
            ;   {timeout, BadTabs}  ->
                    {stop, {error, {timeout_waiting_for_tables, BadTabs}}}
                    
            ;   {error, Reason}     ->
                    {stop, {error, {failed_waiting_for_tables, Reason}}}
            end
            
    ;   {{error, Reason}, _, _} ->
            {stop, {error, Reason}}
    end.
    
    
create_tables([], Created, _, #state{} = State) ->
    {ok, Created, State};
    
create_tables( [Table | Rest], Created, CurrentTables
             , #state{module = CallbackModule} = State)
        when is_atom(Table)  ->
    case catch CallbackModule:table_info(Table) of
        {'EXIT', Reason}        ->
            {{error, Reason}, [], State}
        
    ;   TDef when is_list(TDef) ->
            case proplists:get_value(subscribe, TDef) of
                true        -> gen_server:cast(self(), {?TABLE_SERVICE_TAG, subscribe, Table})
            ;   _Else       -> ok
            end,
            case table_match(Table, CurrentTables, proplists:get_value(attributes, TDef)) of
                exists      -> create_tables(Rest, Created, CurrentTables, State)
            ;   mismatch    -> {{error, {mismatch_table_definition, Table}}, [], State}
            ;   undefined   ->
                    lager:debug("Creating table ~p", [Table]),
                    case mnesia:create_table(Table, proplists:delete(subscribe, TDef)) of
                        {atomic, ok}      ->
                            create_tables(Rest, [Table | Created], CurrentTables, State)
                            
                    ;   {aborted, Reason} ->
                            {{error, {table_creation_failed, Table, Reason}}, [], State}

                    end
            ;   error       -> {{error, bad_table_info}, [], State}
            end
            
    ;   Else                    ->
            {{error, {bad_return_value, Else}}, [], State}
    end.
    

table_match(_, _, undefined) -> error;
    
table_match(Table, Current, Attributes) ->
    case lists:member(Table, Current) of
        true    ->
            case mnesia:table_info(Table, attributes) of
                Attributes  -> exists
            ;   _           -> mismatch
            end
            
    ;   false   -> undefined
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


handle_info({?TABLE_SERVICE_TAG, subscribe, Table}, #state{} = State) ->
    mnesia:subscribe({table, Table, detailed}),
    {noreply, State};
    
    
handle_info({?TABLE_SERVICE_TAG, unsubscribe, Table}, #state{} = State) ->
    mnesia:unsubscribe({table, Table, detailed}),
    {noreply, State};
    
    
%{mnesia_table_event, {write, Table, NewRecord, [OldRecords], ActivityId}}
%handle_write(Table, NewRecord, OldRecord, State)

%{mnesia_table_event, {delete, Table, What, [OldRecords], ActivityId}}
%handle_delete(Table, What, OldRecord, State).

%{write, NewRecord, ActivityId}
%{delete_object, OldRecord, ActivityId}
%{delete, {Tab, Key}, ActivityId}
    
    
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


terminate(_Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


