
-module({{tablesrvid}}).
-compile([{parse_transform, record_info_runtime}]).
-vsn("{{version}}").

-behaviour(table_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/table_server.hrl").


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
    table_server:start_link(?MODULE).
    
    
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



