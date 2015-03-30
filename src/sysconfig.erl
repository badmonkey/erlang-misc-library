
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
load_once(Name) ->
    load_once(Name, atom_to_list(Name)).

    
-spec load_once(atom(), string()) -> ok | {error,any()}.
load_once(Name, FileName) ->
    gen_server:call(?SERVER, {load, Name, FileName}).
    
    

%Type = bool | integer | string | float | atom | list | tuple | ipaddress | path
% get_value(atom|string, Type)
% get_<Type>(atom|string)
% get_as_<Type>(atom|string)


%get_value(readonly, bool)
%get_value("bedrock.server.readonly", bool)





    
%%%%% ------------------------------------------------------- %%%%%


init(_Args) ->
    process_flag(trap_exit, true),
    
    {ok, #state{ modules = dict:new() }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast({load, Name, FileName}, State)
        when  is_atom(Name)
            , is_list(FileName)  ->

    PrivPath = xcode:priv_dir(Name),
    
    FileNameExt =   case filename:extension(FileName) of
                        ?CONFIG_EXT -> FileName
                    ;   _           -> FileName ++ ?CONFIG_EXT)
                    end,
                    
    FilePath =  case filename:pathtype(FileNameExt) of
                    relative    -> filename:join(PrivPath, FileNameExt)
                ;   _           -> FileNameExt
                end,
    
    case filelib:is_regular(FilePath) of
        true    ->
            Parsed = parse(FilePath),
            xerlang:trace(parsed),
            {reply, ok, State#state{ modules = dict:store(Name, Parsed, State#state.modules) }}
            
    ;   _       ->
            {reply, {error, FilePath}, State}
    end;
    
    
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


load_app_config(BaseDir, App)
		where is_atom(App)  ->
	AppConfig = atom_to_list(App) ++ ?CONFIG_EXT,
	FirstFile = filename:join(BaseDir, AppConfig),
	
	case filelib:is_regular(FirstFile) of
		true -> parse(FirstFile)
		
	;	_	->
			AppPath = xcode:priv_dir(Name),
			SecondFile = filename:join(BaseDir, AppConfig),
			
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

