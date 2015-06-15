
-module(snowflake).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-export([start_link/1, start_link/2, get_id/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

         
%
%% this code was based on https://github.com/boundary/flake
%
         

%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { max_time      :: pos_integer()
    , worker_id     :: integer()
    , sequence      :: non_neg_integer()
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


-spec get_id() -> {ok, binary()} | {error, term()}.

get_id() ->
    gen_server:call(?SERVER, {get_id}).

    
start_link(WorkerId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WorkerId], []).

    
start_link(Name, WorkerId) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [WorkerId], []).

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init([WorkerId])
        when  is_integer(WorkerId)
            , WorkerId > 0  ->
    {ok, #state{max_time = xtime:in_milliseconds(), worker_id = WorkerId, sequence = 0}}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({get_id}, _From, State = #state{max_time=MaxTime, worker_id=WorkerId, sequence=Sequence}) ->
    case get(xtime:in_milliseconds(), MaxTime, WorkerId, Sequence, State) of
        {ok, Id, State1}    -> {reply, {ok, Id}, State1}
    ;   {error, E, State1}  -> {reply, {error, E}, State1}
    end;
    

handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_case_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


get(Time, Time, WorkerId, Seq0, State) ->
    Sequence = Seq0 + 1,
    { ok
    , <<Time:64/integer, WorkerId:48/integer, Sequence:16/integer>>
    , State#state{sequence=Sequence} };
    
    
% clock has progressed, reset sequence
get(CurrTime, MaxTime, WorkerId, _, State)
        when CurrTime > MaxTime  ->
    { ok
    , <<CurrTime:64/integer, WorkerId:48/integer, 0:16/integer>>
    , State#state{max_time=CurrTime, sequence=0} };

    
% clock is running backwards
get(CurrTime, MaxTime, _WorkerId, _Sequence, State)
        when MaxTime > CurrTime  ->
    { error
    , clock_running_backwards
    , State }.
  
