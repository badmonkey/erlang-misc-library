
-module(applecore).

-export([]).



%%%%% ------------------------------------------------------- %%%%%


get_archive() ->
	{ok, CWD} = file:get_cwd(),
	AppName = escript:script_name(),
	case filename:pathtype(AppName) of
		absolute	-> AppName
	;	_			-> filename:join( CWD, AppName )
	end.

	


read_file(File) ->
	Fullname = filename:absname(File, get_archive()),
	{ok, Data, _} = erl_prim_loader:get_file(Fullname),
	{ok, Data}.



consult(File) ->
	{ok, Data} = read_file(File),
	{ok, Tokens, _} = erl_scan:string( erlang:binary_to_list(Data) ),
	{ok, Config} = erl_parse:parse_term(Tokens),
	Config.





merge_config(File) ->
	[ merge_app_config(App, Props) || {App, Props} <- consult(File) ].


merge_app_config(App, Props) ->
	io:format("DEBUG ~p~n", [ application:load(App) ]),
	[ load_set_env(App, Par, Value)
	  || {Par, Value} <- proplists:unfold(Props) ].


load_set_env(App, Par, Value) ->
	application:set_env(App, Par, Value).



startup() ->
	archive:merge_config("sys.config"),

	{ok, _} = application:ensure_all_started(lager),

	lager:set_loglevel(lager_console_backend, emergency),
	lager:set_loglevel(lager_file_backend, "error.log", emergency),
	lager:set_loglevel(lager_file_backend, "console.log", emergency),




	{ok, _} = application:ensure_all_started(test_a_call),
	[ io:format("INFO ~s: ~p~n", [K, V])
	  || {K,V} <-	[ {"App", application:get_application() }
					, {"Script", escript:script_name() }
					, {"Args", init:get_arguments() }
					, {"Plain", init:get_plain_arguments() }
					, {"Status", init:get_status() }
					, {"Id", init:script_id() }
					] ],

	


