
-module(sysconfig).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(SYSBOOT_CONFIG, "sysconfig.boot").


-export([start_link/1]).
-export([load_once/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(CONFIG_EXT, ".config").

         

%%%%% ------------------------------------------------------- %%%%%


-record(state,
    {
        config_path
    ,   master  :: atom()
    ,   modules :: sets:set(atom())
    ,   config  :: typed_property:property()
    }).

         
%%%%% ------------------------------------------------------- %%%%%

%
% Opts
% AnchorMap( [A-Z]+  ->  path )
%

start_link(MasterApp)
        when is_atom(MasterApp)  ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MasterApp], []).
    
    
-spec load_once( atom() ) -> type:ok_or_error().

load_once(App)
        when is_atom(App)  ->
    gen_server:call(?SERVER, {load, App}).
      

%Type = bool | integer | string | float | atom | list | tuple | ipaddress | path
% get_value(atom|string, Type)
% get_<Type>(atom|string)
% get_as_<Type>(atom|string)


%get_value(readonly, bool)
%get_value("bedrock.server.readonly", bool)



    
%%%%% ------------------------------------------------------- %%%%%


init([MasterApp]) ->
    process_flag(trap_exit, true),
    
    % only used to start sysconfig (not stored) hence different name
    BootFile = xcode:search_for_file(?SYSBOOT_CONFIG, [config], [MasterApp, erlangx]),
    
    { ok
    , #state{ config_path = BootFile
            , master = MasterApp
            , modules = set:new()
            , config = type_property:new()
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
            Props = parse(AppConfigFile),
            
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
    file:close(InFile),

    config_parser:parse(Acc).

    
loop(InFile, Acc) ->
    case io:request(InFile, {get_until, latin1, prompt, config_lexer, token, [1]}) of
        {ok, Token, _EndLine}   -> loop(InFile, Acc ++ [Token])
    ;   {error, token}          -> {error, scanning_error}
    ;   {eof, _}                -> Acc
    end.

