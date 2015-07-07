
-module({{tablesrvid}}).
-vsn("{{version}}").

-behaviour(table_service).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("erlangx/include/table_service.hrl").


-export([start_link/1, child_spec/2]).

-export([ tables/0, table_info/1]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Table records


-record({{table}}, 
    {
    }).

    
tables() ->
    [{{table}}].
    

table_info(Table) ->
    [ ?FIELDS(Table)
    , ?TABLEDB
    ].

    
%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state, 
    {
    }).
    

%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    table_server:start_link(?MODULE, ?SERVER, []).
    
    
child_spec(Id, Args) -> ?SERVICE_SPEC(Id, ?MODULE, Args).


%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, State) ->
    {stop, {invalid_call_request, Request, From}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_cast(Msg, State) ->
    {stop, {invalid_cast_request, Msg}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_info(Info, State) ->
    {stop, {invalid_info_request, Info}, State}.


%%%%% ------------------------------------------------------- %%%%%


terminate(Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%
% Private Functions



