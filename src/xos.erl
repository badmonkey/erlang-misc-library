
-module(xos).
-extends(os).

-export([get_if_hwaddr/1, get_first_hwaddr/0, find_executable/1, find_executable/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec get_if_hwaddr( string() ) -> {ok, binary()} | {error, term()}.

get_if_hwaddr(undefined) ->
    {error, if_not_found};

get_if_hwaddr("lo") ->
    {error, invalid_interface};
    
get_if_hwaddr(IfName) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    IfProps = proplists:get_value(IfName, IfAddrs),
    case IfProps of
        undefined   -> {error, if_not_found}
        
    ;   _           ->
            case lists:keyfind(hwaddr, 1, IfProps) of
                {hwaddr, HwAddr}    -> {ok, erlang:list_to_binary(HwAddr)}
            ;   false               -> {error, missing_hwaddr_prop}
            end
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec get_first_hwaddr() -> {ok, binary()} | {error, term()}.

get_first_hwaddr() ->
    {ok, IfAddrs} = inet:getifaddrs(),
    find_hwaddr(IfAddrs).

    
-spec find_hwaddr( [{string(), proplists:proplist()}] ) -> {ok, binary()} | {error, term()}.

find_hwaddr([{"lo", _IfConfig}|Rest]) ->
    find_hwaddr(Rest);

find_hwaddr([{_IfName, IfConfig}|Rest]) ->
    case lists:keyfind(hwaddr, 1, IfConfig) of
        {hwaddr, HwAddr}    -> {ok, erlang:list_to_binary(HwAddr)}
    ;   false               -> find_hwaddr(Rest)
    end;
    
find_hwaddr(_) ->
    {error, no_hwaddr_available}.
    

%%%%% ------------------------------------------------------- %%%%%


-spec find_executable( file:filename() ) -> non_existing | file:filename().

find_executable(Name) ->
    case os:find_executable(Name) of
        false   -> find_executable(application:get_application(), Name)
    ;   Else    -> Else
    end.
    

    
-spec find_executable( type:atomlist(), file:filename() ) -> non_existing | file:filename().

% test for
%  App/ebin/Name
%  App/priv/bin/Name
%  App/priv/Name
find_executable(App, Name) when is_atom(App) ->
    FilePath = filename:join( xcode:ebin_dir(App), Name ),
    case filelib:is_regular(FilePath) of
        true    -> FilePath
    ;   _       -> xcode:search_for_file(Name, [bin], App)
    end;

    
find_executable([], _Name) ->    
    non_existing;
    
find_executable([Hd | Rest], Name) ->
    case find_executable(Hd, Name) of
        non_existing    -> find_executable(Rest, Name)
    ;   Else            -> Else
    end;

find_executable(_, _) -> non_existing.

