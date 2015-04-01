
-module(sysconfig).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-export([start_link/0]).
-export([load_once/1, load_once/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(CONFIG_EXT, ".config").

         

%%%%% ------------------------------------------------------- %%%%%


-record(state,
    {
        config_path
    ,   modules :: dict()
    }).

         
%%%%% ------------------------------------------------------- %%%%%

%
% Opts
% AnchorMap( [A-Z]+  ->  path )
%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
    
-spec load_once(atom()) -> ok | {error,any()}.
load_once(Name) when is_atom(Name) ->
    load_once(Name, []).

    
-spec load_once(atom(), [atom()]) -> ok | {error,any()}.
load_once(Name, Requires)
		when  is_atom(Name)
			, is_list(Requires)  ->
    gen_server:call(?SERVER, {load, Name, Requires}).
    
    

%Type = bool | integer | string | float | atom | list | tuple | ipaddress | path
% get_value(atom|string, Type)
% get_<Type>(atom|string)
% get_as_<Type>(atom|string)


%get_value(readonly, bool)
%get_value("bedrock.server.readonly", bool)





    
%%%%% ------------------------------------------------------- %%%%%


init([MasterApp]) ->
    process_flag(trap_exit, true),
    
    { ok
    , #state{
			  config_path = xcode:priv(MasterApp)
			, modules = dict:new()
		}
	}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({load, Name, Requires}, _From, State) ->

    {reply, {error, Name}, State};
    

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


parse_app_config(BaseDir, App)
		when is_atom(App)  ->
	AppConfig = atom_to_list(App) ++ ?CONFIG_EXT,
	FirstFile = filename:join(BaseDir, AppConfig),
	
	case filelib:is_regular(FirstFile) of
		true -> parse(FirstFile)
		
	;	_	->
			AppPath = xcode:priv_dir(App),
			SecondFile = filename:join(AppPath, AppConfig),
			
			case filelib:is_regular(SecondFile) of
				true -> parse(SecondFile)
			;	_	-> throw({error, App, "Can't find config file"})
			end
	end.
	
    
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    file:close(InFile),
    
    xerlang:trace(Acc),
    
    config_parser:parse(Acc).

    
loop(InFile, Acc) ->
    case io:request(InFile, {get_until, latin1, prompt, config_lexer, token, [1]}) of
        {ok, Token, _EndLine}   -> loop(InFile, Acc ++ [Token])
    ;   {error, token}          -> exit(scanning_error)
    ;   {eof, _}                -> Acc
    end.

    
get_value_expand(NameList, Tree) ->    
	ok.