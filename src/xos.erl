
-module(xos).

-export([get_if_hwaddr/1, get_first_hwaddr/0]).


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
    

    