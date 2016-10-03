
-module(applecore).

-export([ start/2, archive/0, alldeps/1
        , halt_success/0, halt_error/2 ]).
%        , loop_forever/1



%%%%% ------------------------------------------------------- %%%%%

%
% /opt/applecore/
% ~/.applecore/
%
% applecore/
%   bin/
%   lib/
%     app1/
%       ebin/
%       priv/
%     ...
%     appN/
%       ebin/
%       priv/
%

%
% /var/opt/applecore/<APP>/<SharedData>
% ~/.applecore/usrdata/<APP>/Mnesia.data
%

% applecore:start( module_with_main
%                , #{ override_app_config => Module | { Module, Func } | fun( (App :: atom()) -> proplists() )
%                   , sys_config => "path/in/archive"
%                   , system_handler => { Module, Func } | fun( (success | error) -> no_return() )
%                   } )
%


% applecore_STATUS = undefined | starting | running
% applecore_INSTALL = system | user | archive
% applecore_APP_DIR = <application directory>
% applecore_HANDLER = fun(What, Options)

% {mnesia, [{dir, "/home/x/data/"}]}


%%%%% ------------------------------------------------------- %%%%%


start(MainApp, Options) ->
    case erlang:put(applecore_STATUS, starting) of
        running     -> ok
    ;   starting    -> halt_error(1, "Impossible!")
    ;   undefined   -> 
                io:format("APPLECORE:init()~n", []),
                start_lager(),
                
                io:format("LOAD: ~p~n", [ application:loaded_applications() ]),
                io:format("WHICH: ~p~n", [ application:which_applications() ]),
                
                io:format("Handlers:lager_event: ~p~n", [ gen_event:which_handlers(lager_event) ]),
                io:format("Handlers:console_lager_event: ~p~n", [ gen_event:which_handlers(console_lager_event) ]),
                io:format("Handlers:audit_lager_event: ~p~n", [ gen_event:which_handlers(audit_lager_event) ]),
                
                io:format("State:lager_event: ~p~n", [ sys:get_state(lager_event) ]),
                io:format("State:console_lager_event: ~p~n", [ sys:get_state(console_lager_event) ]),
                
                
                
                console:debug("First log in start()"),
                lager:debug("Begin startup"),
                
%                error_logger:tty(false),
%                application:ensure_all_started(logger),

%                io:format("APPLECORE:init() pre sysconfig merge~n", []),
                
%                get_and_then(sysconfig, Options,
%                             fun(File) ->
%                                 merge_config(File)
%                             end),

%                io:format("APPLECORE:init() post config merge~n", []),

                io:format("APPLECORE:init() START: ~p~n", [ application:ensure_all_started(MainApp) ]),
                process_flag(trap_exit, true),  
                io:format("APPLECORE:init() done~n", []),
                erlang:put(applecore_STATUS, running)
    end.
    

    
%%%%% ------------------------------------------------------- %%%%%

% { log_root, "/tmp" }
% { colored, true }

start_lager() ->
    ok = application:load(lager),
    
    Applecore = filename:absname("applecore.lager", code:priv_dir(consoletools)),
    case consult(Applecore) of
        {error, _}          -> ok
    ;   {ok, {lager, KVs}}
                when is_list(KVs) ->
            
            [ application:set_env(lager, Par, Value)
                || {Par, Value} <- proplists:unfold(KVs) ]
            
    ;   _                   -> ok
    end,
    
    % Now load startup/lager.config
    
    {ok, _} = application:ensure_all_started(lager).

    
%%%%% ------------------------------------------------------- %%%%%


halt_success() ->
    %lager:wait_for_logging(),
    application:halt().


halt_error(Code, Mesg) ->
    io:format("Error: ~s~n", [Mesg]),
    %logger:wait_for_logging(),
    application:halt(Code).



%%%%% ------------------------------------------------------- %%%%%


archive() ->
    {ok, CWD} = file:get_cwd(),
    AppName = escript:script_name(),
    case filename:pathtype(AppName) of
        absolute    -> AppName
    ;   _           -> filename:join( CWD, AppName )
    end.


%%%%% ------------------------------------------------------- %%%%%
    

% {ok, Binary} | {error, posix() | badarg | terminated | system_limit}
%% @todo more error thing
read_file(File) ->
    Full =  case filename:pathtype(File) of
                absolute    -> File
            ;   relative    -> filename:absname(File, archive())
            end,
    case erl_prim_loader:get_file(Full) of
        {ok, Data, _}   -> {ok, Data}
    ;   error           -> {error, enoent}
    end.


% {ok, Terms} | {error, posix() | badarg | terminated | system_limit | {Line :: integer(), Mod :: module(), Term :: term()}}
%% @todo more error handling better
consult(File) ->
    case read_file(File) of
        {error,_} = E   -> E
    ;   {ok, Data}      ->
            case erl_scan:string( erlang:binary_to_list(Data) ) of  % {error, ErrorInfo :: error_info(), ErrorLocation}
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of    % {error, {erl_anno:line(), module(), error_description()} }
                        {ok, Config}    -> {ok, Config}
                    ;   E               -> E
                    end
            end
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec alldeps( atom() ) -> sets:set( atom() ).

alldeps(App) when is_atom(App) ->
    This = sets:add_element(App, sets:new()),
    case application:load(App) of
        {error, _}  -> sets:new()
    ;   ok          ->
            {ok, Deps} = application:get_key(App, applications),
            DList = [ alldeps(X) || X <- Deps ],
            sets:union([This | DList ])
    end.


%%%%% ------------------------------------------------------- %%%%%


merge_config(File) ->
    [ merge_app_config(App, Props) || {App, Props} <- consult(File) ].


merge_app_config(App, Props) ->
    lager:debug("Load ~p => ~p", [ App, application:load(App) ]),
    [ load_set_env(App, Par, Value)
      || {Par, Value} <- proplists:unfold(Props) ].


load_set_env(App, Par, Value) ->
    application:set_env(App, Par, Value).


%%%%% ------------------------------------------------------- %%%%%


-spec get_and_then( K, #{ K => V }, fun( (V) -> T ) | fun( (K, V) -> T ) ) -> undefined | T.

get_and_then(Key, Map, Then) when is_function(Then, 1) ->
    case maps:get(Key, Map, undefined) of
        undefined   -> undefined
    ;   X           -> Then(X)
    end;

get_and_then(Key, Map, Then) when is_function(Then, 2) ->
    case maps:get(Key, Map, undefined) of
        undefined   -> undefined
    ;   X           -> Then(Key, X)
    end.



%%%%% ------------------------------------------------------- %%%%%


%startup() ->
%    archive:merge_config("sys.config"),
%
%    {ok, _} = application:ensure_all_started(lager),
%
%    lager:set_loglevel(lager_console_backend, emergency),
%    lager:set_loglevel(lager_file_backend, "error.log", emergency),
%    lager:set_loglevel(lager_file_backend, "console.log", emergency),
%
%    {ok, _} = application:ensure_all_started(test_a_call),
%    [ io:format("INFO ~s: ~p~n", [K, V])
%      || {K,V} <-   [ {"App", application:get_application() }
%                    , {"Script", escript:script_name() }
%                    , {"Args", init:get_arguments() }
%                    , {"Plain", init:get_plain_arguments() }
%                    , {"Status", init:get_status() }
%                    , {"Id", init:script_id() }
%                    , {"Deps", sets:to_list( archive:all_deps(test_a_call) ) }
%                   ] ],



%loop_forever( #consoleapp_state{} = State ) ->
%    io:format("Awake/Preparing to hibernate~n"),
%    logger:wait_for_logging(),
%    erlang:hibernate(cmdline, loop_forever, [State]).



