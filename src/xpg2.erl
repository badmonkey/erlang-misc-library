
-module(xpg2).
-extends(pg2).

-export([get_best_pid/1, get_best_pid/2]).


%%%%% ------------------------------------------------------- %%%%%


get_best_pid(Group) ->
    get_best_pid(Group, message_queue_len).
    
    
% really only message_queue_len or reductions make sense
get_best_pid(Group, Info) ->
    Members = pg2:get_members(Group),
    Ranking = lists:map(
                    fun(Pid) ->
                        [{Info, Value}] = erlang:process_info(Pid, [Info]),
                        {Pid, Value}
                    end, Members),
    case lists:keysort(2, Ranking) of
        [{Pid, _} | _] -> Pid;
        [] -> {error, empty_process_group}
    end.
