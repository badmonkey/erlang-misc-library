
-module(gen_server_base).


-export([ call/2, call/3
		, cast/2
		, reply/2
		, abcast/2, abcast/3
		, multi_call/2, multi_call/3, multi_call/4
		, wake_hib/5]).
    

%%%%% ------------------------------------------------------- %%%%%


call(Name, Request) ->
	gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
	gen_server:call(Name, Request, Timeout).
	
cast(Name, Request) ->
	gen_server:cast(Name, Request).

reply(To, Reply) ->
	gen_server:reply(To, Reply).

abcast(Name, Request) ->
	gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
	gen_server:abcast(Nodes, Name, Request).

multi_call(Name, Req) ->
	gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
	gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
	gen_server:multi_call(Nodes, Name, Req, Timeout).

wake_hib(Parent, Name, State, Mod, Debug) ->
	gen_server:wake_hib(Parent, Name, State, Mod, Debug).
  

%%%%% ------------------------------------------------------- %%%%%
