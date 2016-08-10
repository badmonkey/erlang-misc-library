
-module(sysconfig).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(SYSBOOT_CONFIG, "sysconfig.boot").


-export([start_link/1]).
-export([load_app_config/1, get_value/2, get_value/3]).
-export([get_bool/1, get_integer/1, get_string/1, get_float/1, get_atom/1, get_list/1, get_tuple/1, get_ipaddress/1, get_path/1]).
-export([get_as_bool/1, get_as_integer/1, get_as_string/1, get_as_float/1, get_as_atom/1, get_as_list/1, get_as_tuple/1, get_as_ipaddress/1, get_as_path/1]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(CONFIG_EXT, ".config").

         

%%%%% ------------------------------------------------------- %%%%%


-record(state,
    {
      config_path
    , master  :: atom()
    , modules :: sets:set(atom())
    , config  :: typed_property:property()
    }).

         
%%%%% ------------------------------------------------------- %%%%%

%
% Opts
% AnchorMap( [A-Z]+  ->  path )
%

start_link(MasterApp)
        when is_atom(MasterApp)  ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MasterApp], []).
    

%%%%% ------------------------------------------------------- %%%%%

    
-spec load_app_config( atom() ) -> type:ok_or_error().

load_app_config(App)
        when is_atom(App)  ->
    gen_server:call(?SERVER, {load, App}).

    
%%%%% ------------------------------------------------------- %%%%%
    

get_bool(Name)      -> get_value(Name, bool).
get_integer(Name)   -> get_value(Name, integer).
get_string(Name)    -> get_value(Name, string).
get_float(Name)     -> get_value(Name, float).
get_atom(Name)      -> get_value(Name, atom).
get_list(Name)      -> get_value(Name, list).
get_tuple(Name)     -> get_value(Name, tuple).
get_ipaddress(Name) -> get_value(Name, ipaddress).
get_path(Name)      -> get_value(Name, path).


%%%%% ------------------------------------------------------- %%%%%


get_as_bool(Name)       -> get_value(Name, bool).
get_as_integer(Name)    -> get_value(Name, integer).
get_as_string(Name)     -> get_value(Name, string).
get_as_float(Name)      -> get_value(Name, float).
get_as_atom(Name)       -> get_value(Name, atom).
get_as_list(Name)       -> get_value(Name, list).
get_as_tuple(Name)      -> get_value(Name, tuple).
get_as_ipaddress(Name)  -> get_value(Name, ipaddress).
get_as_path(Name)       -> get_value(Name, path).


%%%%% ------------------------------------------------------- %%%%%


get_value(Name, Type) -> get_value(Name, Type, undefined).

get_value(Name, Type, Default) ->
    case get_raw_value(Name) of
        {Type, Value}   -> Value
    ;   Else            -> Default
    end.
    

get_raw_value(Name) ->
    gen_server:call(?SERVER, {get_raw_value, Name}).

    
%%%%% ------------------------------------------------------- %%%%%


init([MasterApp]) ->
    process_flag(trap_exit, true),
    
    % only used to start sysconfig (not stored) hence different name
    BootFile = xcode:search_for_file(?SYSBOOT_CONFIG, [config], [MasterApp, erlangx]),
    
    { ok
    , #state{ config_path = BootFile
            , master = MasterApp
            , modules = sets:new()
            , config = typed_property:new()
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {load, App}, _From
           , #state{modules = Loaded, config = Config, master = MasterApp} = State) ->
    
    case sets:is_element(App, Loaded) of
        true    -> {reply, ok, State}
        
    ;   false   ->
            AppConfig = atom_to_list(App) ++ ?CONFIG_EXT,
            AppConfigFile = xcode:search_for_file(AppConfig, [config], [App, MasterApp]),
            {ok, Props} = parse(AppConfigFile),
            
            Merged = typed_property:merge(Config, Props),
            Expanded = typed_property:expand(Merged),
            
            { reply, ok
            , State#state{
                      modules = sets:add_element(App, Loaded)
                    , config = Expanded
                    } }
    end;
    
    
handle_call( {reload, App}, From, State) ->
    handle_call( {load, App}, From, State#state{ modules = sets:del_element(App, State#state.modules) } );
    
    
handle_call( {get_raw_value, Name}, From, #state{config = Config} = State) ->
    Value = typed_property:get_raw_value(Config, Name),
    {reply, Value, State};
    

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


parse(non_existing) -> undefined;
    
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    _ = file:close(InFile),

    config_parser:parse(Acc).

    
loop(InFile, Acc) ->
    case io:request(InFile, {get_until, latin1, prompt, config_lexer, token, [1]}) of
        {ok, Token, _EndLine}   -> loop(InFile, Acc ++ [Token])
    ;   {error, token}          -> {error, scanning_error}
    ;   {eof, _}                -> Acc
    end.

