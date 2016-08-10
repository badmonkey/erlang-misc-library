
-module(consoleapp).

-export([build/1, build/2, process/2, application_halt/2, usage/1, usage/2, usage/3]).
-export([get_global_options/1, get_options/1, get_unused_args/1, get_appname/1, get_command/1, get_version/1]).

%set_unused_args()


-define(CONSOLE_COMMAND, 'consoleapp$command').



%%%%% ------------------------------------------------------- %%%%%


-record(consoleapp_state,
    { appname           = ""        :: string()
    , command           = ""        :: string()
    , version           = undefined :: undefined | string()
    , check_args        = true      :: boolean()
    , mode              = basic     :: basic | extended
    , scope             = global    :: global | command
    , auto_help         = true      :: boolean
    , auto_version      = true      :: boolean
    , getopt_spec       = []        :: [getopt:option_spec()]
    , positional_spec   = []        :: [getopt:option_spec()]
    , options           = []        :: [option_value()]
    , unused_args       = []        :: [term()]
    , callbacks         = #{}       :: #{ atom => arg_callback_spec() }
    , command_ids       = []        :: [string()]
    , command_names     = []        :: [string()]
    , command_alias     = #{}       :: #{ Alias :: string() => Cmd :: string() }
    , command_map       = #{}       :: #{ Cmd :: undefined | string() => arg_command_spec() }
    , command_info      = #{}       :: #{ CmdId :: string() => {Help :: string(), Alias :: [string()]} }
    , global_options    = []        :: [option_value()]
    }).


%%%%% ------------------------------------------------------- %%%%%


-type arg_type() :: getopt:arg_type().
-type arg_value() :: getopt:arg_value().

-type option_value() :: atom() | {atom(), arg_value()}.


-type arg_type_spec() :: arg_type() | { arg_type(), arg_value() }.
-type arg_name_spec() :: char() | string() | { char(), string() }.

-type arg_callback_spec() :: fun( ( arg_value() ) -> _ )
                           | fun( ( atom(), arg_value() ) -> _ ).
                           
-type arg_command_spec() :: module()
                          | fun( ( #consoleapp_state{}, [term()] ) -> _ ).


-type arg_setting_spec() :: {appname, string()}
                          | {version, string()}
                          | {check_args, boolean()}.
                          
-type arg_option_spec() :: arg_setting_spec()
                         | {option, Id :: atom(), Type :: arg_type_spec(), Name :: arg_name_spec(), Help :: string()}
                         | {option, Id :: atom(), Type :: arg_type_spec(), Name :: arg_name_spec(), Help :: string(), arg_callback_spec()}
                         | {positional, Id :: atom(), Type :: arg_type_spec(), Help :: string()}
                         | {positional, Id :: atom(), Type :: arg_type_spec(), Help :: string(), arg_callback_spec()}
                         | {command, undefined, arg_command_spec()}
                         | {command, string() | [string()], arg_command_spec(), Help :: string()}.
                        
-type arg_build_spec() :: [arg_option_spec()].


-export_type([ arg_type/0, arg_value/0, option_value/0
             , arg_type_spec/0, arg_name_spec/0, arg_callback_spec/0, arg_command_spec/0
             , arg_setting_spec/0, arg_option_spec/0, arg_build_spec/0]).


%%%%% ------------------------------------------------------- %%%%%


-spec build( arg_build_spec() ) -> #consoleapp_state{}.

build(CmdlineSpec) ->
    State = lists:foldl( fun build_item/2, #consoleapp_state{}, CmdlineSpec ),
    
    Mode =  case { length(State#consoleapp_state.positional_spec)
                 , length(State#consoleapp_state.command_names) } of
                {_, 0}  -> basic
            ;   {0, _}  -> extended
            ;   {_, _}  -> application_halt(State, "Can't have both positional and command options")
            end,
    
    SortedIds = lists:sort(State#consoleapp_state.command_ids),
    SortedNames = lists:sort(State#consoleapp_state.command_names),
    SortedOptions = lists:keysort(1, State#consoleapp_state.getopt_spec),
    
    State#consoleapp_state{
            mode = Mode,
            getopt_spec = SortedOptions,
            command = State#consoleapp_state.appname,
            command_ids = SortedIds,
            command_names = SortedNames
        }.
    

-spec build( #consoleapp_state{}, arg_build_spec() ) -> #consoleapp_state{}.

build(#consoleapp_state{} = Parent, CmdlineSpec) ->
    NewState = build(CmdlineSpec),
    NewState#consoleapp_state{
            scope = command,
            appname = Parent#consoleapp_state.appname,
            command = Parent#consoleapp_state.command,
            global_options = Parent#consoleapp_state.global_options
        }.
    
    
%%%%% ------------------------------------------------------- %%%%%
                

-spec build_item( arg_option_spec(), #consoleapp_state{} ) -> no_return() | #consoleapp_state{}.

build_item({appname, Name}, #consoleapp_state{} = State)            ->
    State#consoleapp_state{ appname = Name };
    
build_item({version, Version}, #consoleapp_state{} = State)         ->
    State#consoleapp_state{ version = Version };

build_item({check_args, B}, #consoleapp_state{} = State)            ->
    State#consoleapp_state{ check_args = B };
    
    
build_item( {option, Id, Type, Name, Help}
          , #consoleapp_state{ getopt_spec = Spec } = State)        ->
    {Short, Long} = case Name of
                        {_, _} = SL             -> SL
                    ;   S when is_integer(S)    -> {S, undefined}
                    ;   L when is_list(L)       -> {undefined, L}
                    end,
                    
    {AutoHelp, AutoVersion} =   case Long of
                                    "help"      -> {false, State#consoleapp_state.auto_version}
                                ;   "version"   -> {State#consoleapp_state.auto_help, false}
                                ;   _           -> {State#consoleapp_state.auto_help, State#consoleapp_state.auto_version}
                                end,

    State#consoleapp_state{
            auto_help = AutoHelp,
            auto_version = AutoVersion,
            getopt_spec = [{Id, Short, Long, map_type(Type), Help} | Spec]
        };
    
    
build_item( {option, Id, Type, Name, Help, Callback}
          , #consoleapp_state{ getopt_spec = Spec } = State)        ->
    {Short, Long} = case Name of
                        {_, _} = SL             -> SL
                    ;   S when is_integer(S)    -> {S, undefined}
                    ;   L when is_list(L)       -> {undefined, L}
                    end,
    % @todo what about auto_help/auto_version?
    State#consoleapp_state{
            getopt_spec = [{Id, Short, Long, map_type(Type), Help} | Spec],
            callbacks = maps:put(Id, Callback, State#consoleapp_state.callbacks)
        };

        
build_item( {positional, Id, Type, Help}
          , #consoleapp_state{ positional_spec = Spec } = State)    ->
    State#consoleapp_state{
            positional_spec = Spec ++ [{Id, undefined, undefined, Type, Help}]
        };
    
    
build_item( {positional, Id, Type, Help, Callback}
          , #consoleapp_state{ positional_spec = Spec } = State)    ->
    State#consoleapp_state{
            positional_spec = Spec ++ [{Id, undefined, undefined, Type, Help}],
            callbacks = maps:put(Id, Callback, State#consoleapp_state.callbacks)
        };

    
build_item( {command, undefined, Command}
          , #consoleapp_state{ command_map = Cmds } = State)        ->
    State#consoleapp_state{
            command_map = maps:put(undefined, Command, Cmds)
        };            
        
        
build_item( {command, [], _, _}
          , #consoleapp_state{} = State)                            ->
    exit(State, "empty name argument for {command}");        

        
build_item( {command, CmdName, Command, Help}
          , #consoleapp_state{ command_map = Cmds
                             , command_ids = Ids
                             , command_names = Names
                             , command_alias = Alias
                             , command_info = Info } = State)       ->
    case hd(CmdName) of
        X when is_integer(X)        ->
            State#consoleapp_state{
                    command_map = maps:put(CmdName, Command, Cmds),
                    command_ids = [CmdName | Ids],
                    command_names = [CmdName | Names],
                    command_alias = maps:put(CmdName, CmdName, Alias),
                    command_info = maps:put(CmdName, {Help, []}, Info)
                }
        
    ;   TheCmd when is_list(TheCmd) ->
            NewAlias =   lists:foldl(
                              fun(X, M) ->
                                maps:put(X, TheCmd, M)
                              end
                            , Alias
                            , CmdName),
                                
            State#consoleapp_state{
                    command_map = maps:put(TheCmd, Command, Cmds),
                    command_ids = [TheCmd | Ids],
                    command_names = CmdName ++ Names,
                    command_alias = NewAlias,
                    command_info = maps:put(TheCmd, {Help, tl(CmdName)}, Info)
                }
    
    ;   _Something              -> application_halt(State, "invalid argument for {command}: ~p", [CmdName])
    end;
    

build_item(X, #consoleapp_state{} = State) ->
    application_halt(State, "invalid option: ~p", [X]).

    
    
map_type(boolean) -> undefined;
map_type({boolean, false}) -> undefined;
map_type(X) -> X.
    

%%%%% ------------------------------------------------------- %%%%%


-spec process( #consoleapp_state{}, [term()] ) -> no_return() | #consoleapp_state{}.

process( #consoleapp_state{ scope = global } = State, [] ) ->
    usage(State),
    application_halt(State, 0);
    
    
process( #consoleapp_state{ scope = command } = State, [] ) ->
    State;
    
    
process( #consoleapp_state{ mode = basic } = State, Args ) ->
    Parse = case State#consoleapp_state.check_args of
                true    -> getopt:parse_and_check(make_getopt_spec(State), Args)
            ;   false   -> getopt:parse(make_getopt_spec(State), Args)
            end,
    case Parse of
        {ok, {Options, Unused}}     ->
            process_basic(Options, Unused, State)
            
    ;   {error, {Error, Option}}    ->
            usage(State, "Error ~p ~p", [Error, Option]),
            application_halt(State, 1)
    end;
    
    
process( #consoleapp_state{ mode = extended
                          , command_names = CmdNames } = State, Args ) ->
    {WorkingArgs, SplitUnused} = split_args(Args, CmdNames),
    Parse = case State#consoleapp_state.check_args of
                true    -> getopt:parse_and_check(make_getopt_spec(State), WorkingArgs)
            ;   false   -> getopt:parse(make_getopt_spec(State), WorkingArgs)
            end,
    case Parse of
        {ok, {Options, Unused}}     ->
            NewState = process_basic(Options, Unused ++ SplitUnused, State),
            process_extended(NewState)
            
    ;   {error, {Error, Option}}    ->
            usage(State, "Error ~p ~p", [Error, Option]),
            application_halt(State, 1)
    end.

    
process_basic( Options, Unused
             , #consoleapp_state{ callbacks = Callbacks } = State ) ->
    NewState =  case State#consoleapp_state.scope of
                    global      ->
                            State#consoleapp_state {
                                options = Options,
                                global_options = proplists:delete(?CONSOLE_COMMAND, Options),
                                unused_args = Unused
                            }

                ;   command     ->
                        State#consoleapp_state {
                                options = Options,
                                unused_args = Unused
                            }
                end,
    process_help(NewState),
    process_version(NewState),

    lists:foreach(
        fun (Name)          when is_atom(Name)  -> handle_callback(Name, true,  maps:get(Name, Callbacks, undefined))
        ;   ({Name, Value}) when is_atom(Name)  -> handle_callback(Name, Value, maps:get(Name, Callbacks, undefined))
        end, Options),
    NewState.
    
    
process_extended( #consoleapp_state{ options = Options
                                   , command_alias = Alias
                                   , command_map = Commands } = State ) ->
    case proplists:get_value(?CONSOLE_COMMAND, Options) of
        undefined   ->
            case maps:get(undefined, Commands, undefined) of
                undefined   ->
                    usage(State),
                    application_halt(State, 0)
                    
            ;   Cmd         ->
                    handle_command("<default>", Cmd, State)
            end
            
    ;   CmdName     ->
            RealCmd = maps:get(CmdName, Alias, undefined),
            case maps:get(RealCmd, Commands, undefined) of
                undefined   ->
                    usage(State, "Invalid command ~s", [CmdName]),
                    application_halt(State, 1)
                    
            ;   Cmd         ->
                    handle_command(RealCmd, Cmd, State)
            end
    end.
    
   
process_help( #consoleapp_state{ auto_help = true
                               , options = Options} = State) ->
    case proplists:get_bool(help, Options) of
        false   -> ok
    ;   true    ->
            usage(State),
            application_halt(State, 0)
    end.
    
    
process_version( #consoleapp_state{ auto_version = true
                                  , options = Options} = State) ->
    case proplists:get_bool(version, Options) of
        false   -> ok
    ;   true    ->
            io:format("~s version ~s~n", [ State#consoleapp_state.command
                                         , State#consoleapp_state.version]),
            application_halt(State, 0)
    end.    
    

%%%%% ------------------------------------------------------- %%%%%


handle_callback(_, _, undefined) -> ok;

% @TODO add try catch
handle_callback(_, Value, Callback)
        when is_function(Callback,1)  ->
    Callback(Value);
    
    
handle_callback(Name, Value, Callback)
        when is_function(Callback,2)  ->
    Callback(Name, Value).
            
            
%%%%% ------------------------------------------------------- %%%%%


% @TODO add try/catch
handle_command( Name, Module
              , #consoleapp_state{} = State )
        when is_atom(Module) ->
    NewState = push_new_command(State, Name),
    %erlang:function_exported(Module, Function, Arity) -> boolean
    case apply(Module, main, [NewState, State#consoleapp_state.unused_args]) of
        #consoleapp_state{} = S -> pop_command(State, S)
    ;   _                       -> State
    end;
    
    
handle_command( Name, Func
              , #consoleapp_state{} = State )
        when is_function(Func,2) ->
    NewState = push_new_command(State, Name),
    case Func(NewState, State#consoleapp_state.unused_args) of
        #consoleapp_state{} = S -> pop_command(State, S)
    ;   _                       -> State
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec application_halt( #consoleapp_state{}, integer() | abort | string() ) -> no_return().

application_halt( #consoleapp_state{} = _State, Return ) when is_list(Return) ->
    io:format(Return ++ "~n"),
    erlang:halt(1);
    
application_halt( #consoleapp_state{} = _State, Return ) ->
    erlang:halt(Return).
    
    
application_halt(State, Msg, Args) ->
    application_halt(State, io_lib:format(Msg, Args)).    


%%%%% ------------------------------------------------------- %%%%%


-spec usage( #consoleapp_state{} ) -> ok.

usage( #consoleapp_state{ mode = basic } = State ) ->
    getopt:usage( make_getopt_spec(State)
                , State#consoleapp_state.command);

                
usage( #consoleapp_state{ mode = extended
                        , command_ids = Ids
                        , command_info = Info } = State ) ->

    CommandHelp =   lists:foldl(
                          fun(Nm, Acc) ->
                            HelpItem =  case maps:get(Nm, Info, undefined) of
                                            undefined       -> application_halt(State, "No info for command ~p", [Nm])
                                
                                        ;   {Help, []}      -> {Nm, Help}
                            
                                        ;   {Help, Alias}   ->
                                                NewHelp = io_lib:format("[~s] ~s", [string:join(Alias, ", "), Help]),
                                                {Nm, NewHelp}
                                        end,
                            [HelpItem | Acc]
                          end
                        , [{"--------", ""}, {"Commands", ""}, {"\n\n", ""}], Ids),
                        
    getopt:usage( make_auto_options(State) ++ State#consoleapp_state.getopt_spec
                , State#consoleapp_state.command
                , "command ..."
                , lists:reverse(CommandHelp)).
                


-spec usage( #consoleapp_state{}, string() ) -> ok.    
    
usage( #consoleapp_state{} = State, Msg ) ->
    io:format(standard_error, "~s~n", [Msg]),
    usage(State).

    
-spec usage( #consoleapp_state{}, string(), [term()] ) -> ok.    
    
usage( #consoleapp_state{} = State, Msg, Args ) when is_list(Args) ->
    io:format(standard_error, Msg, Args),
    io:format(standard_error, "~n", []),
    usage(State).
    

%%%%% ------------------------------------------------------- %%%%%


make_getopt_spec( #consoleapp_state{ mode = basic } = State ) ->
    make_auto_options(State) ++ State#consoleapp_state.getopt_spec ++ State#consoleapp_state.positional_spec;
    
make_getopt_spec( #consoleapp_state{ mode = extended } = State ) ->
    make_auto_options(State) ++ State#consoleapp_state.getopt_spec ++ [{?CONSOLE_COMMAND, undefined, undefined, undefined, undefined}].
    
    
make_auto_options( #consoleapp_state{ auto_help = AutoHelp
                                    , auto_version = AutoVersion
                                    , version = Version
                                    , appname = Appname } ) ->
    case AutoHelp of
        true    -> [{help, undefined, "help", undefined, "Display this information"}]
    ;   false   -> []
    end ++  case {AutoVersion, Version} of
                {true, undefined}   -> []
            ;   {true, _}           -> [{version, undefined, "version", undefined, "Display " ++ Appname ++ " version information"}]
            ;   {false, _}          -> []
            end.
    

%%%%% ------------------------------------------------------- %%%%%


pop_command( #consoleapp_state{} = Current
           , #consoleapp_state{} = Parent ) ->
    Current#consoleapp_state{
            unused_args = Parent#consoleapp_state.unused_args
        }.
    
    
push_new_command( #consoleapp_state{} = Current
                , Name ) ->
    Current#consoleapp_state{
              command = string:join([Current#consoleapp_state.command, Name], " ")
            , mode = basic
            , scope = command
            , getopt_spec = []
            , positional_spec = []
            , callbacks = #{}
            , command_names = []
            , command_map = #{}
            , command_info = #{}
        }.    


%%%%% ------------------------------------------------------- %%%%%


split_args(Args, Commands) ->
    split_args(Args, Commands, []).
    
    
split_args([], _, WorkingArgs) ->
    { lists:reverse(WorkingArgs)
    , []
    };
    
    
split_args([X | Tail], Commands, WorkingArgs) ->
    case lists:member(X, Commands) of
        false   -> split_args(Tail, Commands, [X | WorkingArgs])
    ;   true    ->
            { lists:reverse([X | WorkingArgs])
            , Tail
            }
    end.


%%%%% ------------------------------------------------------- %%%%%


get_global_options( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.global_options).
    
    
get_options( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.options).
    
    
get_unused_args( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.unused_args).
    
    
get_appname( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.appname).
    
    
get_command( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.command).
    
    
get_version( #consoleapp_state{} = State ) ->
    (State#consoleapp_state.version).
    
    