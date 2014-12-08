

-module(erlx_server).


%% API
-export([start/3, start/4,
         start_link/3, start_link/4,
         call/3, call/4,
         cast/3, reply/2,
         enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% System exports
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         system_get_state/1,
         system_replace_state/2,
         format_status/2]).

%% Internal exports
-export([init_it/6]).

-import(error_logger, [format/2]).


%%%=========================================================================
%%%  API
%%%=========================================================================

-type server_cmd() :: {clear} | {pop} | {push, atom()}.
    
-type noreply_type() ::
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), timeout() | hibernate} |
        {noreply, NewState :: term(), server_cmd()} |
        {noreply, NewState :: term(), timeout() | hibernate, server_cmd()}.

    
-type reply_type() ::
        {reply, Reply :: term(), NewState :: term()} |
        {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
        {reply, Reply :: term(), NewState :: term(), server_cmd()} |
        {reply, Reply :: term(), NewState :: term(), timeout() | hibernate, server_cmd()}.
        
-type stop_type() :: {stop, Reason :: term(), NewState :: term()}.
    

-callback init(Args :: term()) ->
        {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
        {stop, Reason :: term()} | ignore.
        
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
        reply_type() | noreply_type() | stop_type() |
        {stop, Reason :: term(), Reply :: term(), NewState :: term()}.
        
-callback handle_cast(Request :: term(), State :: term()) -> noreply_type() | stop_type().
-callback handle_info(Info :: timeout | term(), State :: term()) -> noreply_type() | stop_type().
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
        term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
        {ok, NewState :: term()} | {error, Reason :: term()}.


    
%%%  -----------------------------------------------------------------


start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


%% ----------------------------------------------------------------- 


call(Name, Category, Request) ->
    case catch gen:call(Name, {'$gen_call', Category}, Request) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

    
call(Name, Category, Request, Timeout) ->
    case catch gen:call(Name, {'$gen_call', Category}, Request, Timeout) of
    {ok,Res} ->
        Res;
    {'EXIT',Reason} ->
        exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.



%% -----------------------------------------------------------------

cast({global,Name}, Category, Request) ->
    catch global:send(Name, cast_msg(Category, Request)),
    ok;
cast({via, Mod, Name}, Category, Request) ->
    catch Mod:send(Name, cast_msg(Category, Request)),
    ok;
cast({Name,Node}=Dest, Category, Request) when is_atom(Name), is_atom(Node) -> 
    do_cast(Dest, Category, Request);
cast(Dest, Category, Request) when is_atom(Dest) ->
    do_cast(Dest, Category, Request);
cast(Dest, Category, Request) when is_pid(Dest) ->
    do_cast(Dest, Category, Request).

do_cast(Dest, Category, Request) -> 
    do_send(Dest, cast_msg(Category, Request)),
    ok.
    
cast_msg(Category, Request) -> {'$gen_cast',Category,Request}.


%% -----------------------------------------------------------------


reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.


%% -----------------------------------------------------------------


enter_loop(Mod, Options, State) ->
    enter_loop(Mod, Options, State, self(), infinity).

enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when Scope == local; Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, ServerName = {via, _, _}) ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, Timeout) ->
    enter_loop(Mod, Options, State, self(), Timeout).

enter_loop(Mod, Options, State, ServerName, Timeout) ->
    Name = get_proc_name(ServerName),
    Parent = get_parent(),
    Debug = debug_options(Name, Options),
    loop(Parent, Name, {[], State}, Mod, Timeout, Debug, false).    %% TODO new srver state
    


%%%========================================================================
%%% Gen-callback functions
%%%========================================================================


init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = debug_options(Name, Options),
    case catch Mod:init(Args) of
    {ok, State} ->
        proc_lib:init_ack(Starter, {ok, self()}), 	    
        loop(Parent, Name, {[], State}, Mod, infinity, Debug, false);   %% TODO new srver state
    {ok, State, Timeout} ->
        proc_lib:init_ack(Starter, {ok, self()}), 	    
        loop(Parent, Name, {[], State}, Mod, Timeout, Debug, false);     %% TODO new srver state
    {stop, Reason} ->
        %% For consistency, we must make sure that the
        %% registered name (if any) is unregistered before
        %% the parent process is notified about the failure.
        %% (Otherwise, the parent process could get
        %% an 'already_started' error if it immediately
        %% tried starting the process again.)
        unregister_name(Name0),
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
    ignore ->
        unregister_name(Name0),
        proc_lib:init_ack(Starter, ignore),
        exit(normal);
    {'EXIT', Reason} ->
        unregister_name(Name0),
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
    Else ->
        Error = {bad_return_value, Else},
        proc_lib:init_ack(Starter, {error, Error}),
        exit(Error)
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    _ = (catch unregister(Name));
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name);
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.
    

%%% ---------------------------------------------------


loop(Parent, Name, State, Mod, hibernate, Debug, _) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, Debug]);
    
loop(Parent, Name, {SrvrState, ClntState} = State, Mod, Time, Debug, Hib) ->
    Category = bugnutz,
    ProcessAll = false,
    {RFrom, RprtMsg, Reply} = receive
                    %% system
                {system, From, Req} = M                 ->
                    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug, [Name, State, Mod, Time], Hib),
                    {undefined, M, system}
            ;   {'EXIT', Parent, Reason} = M            ->
                    terminate(Reason, Name, M, Mod, ClntState, Debug)
            
                    %% gen_call
            ;   {'$gen_call', From, Msg}                -> {From, Msg, catch Mod:handle_call(Msg, From, ClntState)}
            ;   {{'$gen_call', always}, From, Msg}      -> {From, Msg, catch Mod:handle_call(Msg, From, ClntState)}
            ;   {{'$gen_call', Category}, From, Msg}    -> {From, Msg, catch Mod:handle_call(Msg, From, ClntState)}
            
                    %% gen_cast
            ;   {'$gen_cast', Msg}                      -> {undefined, Msg, Mod:handle_cast(Msg, ClntState)}
            ;   {'$gen_cast', always, Msg}              -> {undefined, Msg, Mod:handle_cast(Msg, ClntState)}
            ;   {'$gen_cast', Category, Msg}            -> {undefined, Msg, Mod:handle_cast(Msg, ClntState)}
            
                    %% common system info
            ;   {'DOWN', _, _, _, _} = M                -> {undefined, M, Mod:handle_info(M, ClntState)}
            ;   {timeout, _, _} = M                     -> {undefined, M, Mod:handle_info(M, ClntState)}
            ;   {nodedown, _} = M                       -> {undefined, M, Mod:handle_info(M, ClntState)}
            
                    %% catch all
            ;   Info when ProcessAll                    -> {undefined, Info, Mod:handle_info(Info, ClntState)}
            
            after Time ->
                {timeout, timeout, timeout}
            end,
            
    case Reply of
        system -> ok
        
            %% gen_call
    ;   {reply, Reply, NState} when RFrom =/= undefined ->
            Debug1 = debug_reply(Name, RFrom, Reply, NState, Debug),
            loop(Parent, Name, {SrvrState, NState}, Mod, infinity, Debug1, Hib)
    ;   {reply, Reply, NState, Time1} when RFrom =/= undefined ->
            Debug1 = debug_reply(Name, RFrom, Reply, NState, Debug),
            loop(Parent, Name, {SrvrState, NState}, Mod, Time1, Debug1, Hib)
            
    ;   {stop, RReason, Reply, NState} when RFrom =/= undefined ->
            {'EXIT', R} = (catch terminate(RReason, Name, RprtMsg, Mod, NState, Debug)),
            _ = debug_reply(Name, RFrom, Reply, NState, Debug),
            exit(R)
            
            %% common
    ;   {noreply, NState} when Debug =/= [] ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {noreply, NState}),
            loop(Parent, Name, {SrvrState, NState}, Mod, infinity, Debug1, Hib)
    ;   {noreply, NState, Time1} when Debug =/= [] ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {noreply, NState}),
            loop(Parent, Name, {SrvrState, NState}, Mod, Time1, Debug1, Hib)
        
    ;   {noreply, NState} ->
            loop(Parent, Name, {SrvrState, NState}, Mod, infinity, [], Hib)
    ;   {noreply, NState, Time1} ->
            loop(Parent, Name, {SrvrState, NState}, Mod, Time1, [], Hib)
            
            %% stopping/system
    ;   {stop, RReason, NState} ->
            terminate(RReason, Name, RprtMsg, Mod, NState, Debug)
    
    ;   {'EXIT', What} ->
            terminate(What, Name, RprtMsg, Mod, ClntState, Debug)
            
            %% catch all
    ;   _ ->
            terminate({bad_return_value, Reply}, Name, RprtMsg, Mod, ClntState, Debug)
        
    end.

   
wake_hib(Parent, Name, State, Mod, Debug) ->
    loop(Parent, Name, State, Mod, hibernate, Debug, true).


    
debug_reply(_Name, From, Reply, _State, []) ->
    reply(From, Reply),
    [];    
debug_reply(Name, From, Reply, State, Debug) ->
    reply(Name, From, Reply, State, Debug).
    
reply(Name, {To, Tag}, Reply, State, Debug) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
            {out, Reply, To, State} ).

   
%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    case catch erlang:send(Dest, Msg, [noconnect]) of
    noconnect ->
        spawn(erlang, send, [Dest,Msg]);
    Other ->
        Other
    end.



%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, State, Mod, Time]) ->
    loop(Parent, Name, State, Mod, Time, Debug, false).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time]) ->
    terminate(Reason, Name, [], Mod, State, Debug).

system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of         %% TODO State needs working on
        {ok, NewState} -> {ok, [Name, NewState, Mod, Time]}
    ;   Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time]) ->        %% TODO State needs working on
    {ok, State}.

system_replace_state(StateFun, [Name, State, Mod, Time]) ->
    NState = StateFun(State),                           %% TODO State needs working on
    {ok, NState, [Name, NState, Mod, Time]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
    {'$gen_call', {From, _Tag}, Call} ->
        io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
              [Name, Call, From]);
    {'$gen_cast', Cast} ->
        io:format(Dev, "*DBG* ~p got cast ~p~n",
              [Name, Cast]);
    _ ->
        io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n", 
        [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, Name, Msg, Mod, State, Debug) ->
    case catch Mod:terminate(Reason, State) of
    {'EXIT', R} ->
        error_info(R, Name, Msg, State, Debug),
        exit(R);
    _ ->
        case Reason of
        normal ->
            exit(normal);
        shutdown ->
            exit(shutdown);
        {shutdown,_}=Shutdown ->
            exit(Shutdown);
        _ ->
            FmtState =
            case erlang:function_exported(Mod, format_status, 2) of
                true ->
                Args = [get(), State],
                case catch Mod:format_status(terminate, Args) of
                    {'EXIT', _} -> State;
                    Else -> Else
                end;
                _ ->
                State
            end,
            error_info(Reason, Name, Msg, FmtState, Debug),
            exit(Reason)
        end
    end.

error_info(_Reason, application_controller, _Msg, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Reason, Name, Msg, State, Debug) ->
    Reason1 = 
    case Reason of
        {undef,[{M,F,A,L}|MFAs]} ->
        case code:is_loaded(M) of
            false ->
            {'module could not be loaded',[{M,F,A,L}|MFAs]};
            _ ->
            case erlang:function_exported(M, F, length(A)) of
                true ->
                Reason;
                false ->
                {'function not exported',[{M,F,A,L}|MFAs]}
            end
        end;
        _ ->
        Reason
    end,    
    format("** Generic server ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
        [Name, Msg, State, Reason1]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Name, Opts) ->
    case opt(debug, Opts) of
    {ok, Options} -> dbg_options(Name, Options);
    _ -> dbg_options(Name, [])
    end.

dbg_options(Name, []) ->
    Opts = 
    case init:get_argument(generic_debug) of
        error ->
        [];
        _ ->
        [log, statistics]
    end,
    dbg_opts(Name, Opts);
dbg_options(Name, Opts) ->
    dbg_opts(Name, Opts).

dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
    {'EXIT',_} ->
        format("~p: ignoring erroneous debug options - ~p~n",
           [Name, Opts]),
        [];
    Dbg ->
        Dbg
    end.

get_proc_name(Pid) when is_pid(Pid) ->
    Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
    {registered_name, Name} ->
        Name;
    {registered_name, _Name} ->
        exit(process_not_registered);
    [] ->
        exit(process_not_registered)
    end;    
get_proc_name({global, Name}) ->
    case global:whereis_name(Name) of
    undefined ->
        exit(process_not_registered_globally);
    Pid when Pid =:= self() ->
        Name;
    _Pid ->
        exit(process_not_registered_globally)
    end;
get_proc_name({via, Mod, Name}) ->
    case Mod:whereis_name(Name) of
    undefined ->
        exit({process_not_registered_via, Mod});
    Pid when Pid =:= self() ->
        Name;
    _Pid ->
        exit({process_not_registered_via, Mod})
    end.

get_parent() ->
    case get('$ancestors') of
    [Parent | _] when is_pid(Parent)->
            Parent;
        [Parent | _] when is_atom(Parent)->
            name_to_pid(Parent);
    _ ->
        exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
    undefined ->
        case global:whereis_name(Name) of
        undefined ->
            exit(could_not_find_registered_name);
        Pid ->
            Pid
        end;
    Pid ->
        Pid
    end.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time]] = StatusData,
    Header = gen:format_status_header("Status for generic server",
                                      Name),
    Log = sys:get_debug(log, Debug, []),
    DefaultStatus = [{data, [{"State", State}]}],
    Specfic =
    case erlang:function_exported(Mod, format_status, 2) of
        true ->
        case catch Mod:format_status(Opt, [PDict, State]) of
            {'EXIT', _} -> DefaultStatus;
                    StatusList when is_list(StatusList) -> StatusList;
            Else -> [Else]
        end;
        _ ->
        DefaultStatus
    end,
    [{header, Header},
     {data, [{"Status", SysState},
         {"Parent", Parent},
         {"Logged events", Log}]} |
     Specfic].
