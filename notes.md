


%example: justquick (actor) closed (verb) issue 2 (object) on activity-stream (target) 12 hours ago
%action(Actor, Verb, [Action_object], [Target])


cf push -b https://github.com/spiegela/cf-buildpack-erlang



upgrade(OldVsn, NextVsn, State) ->
    NextState = code_change(NextVsn, State),
    case code_change({down, OldVsn}, NextState) of
        State -> {ok, NextState};
        _ -> {error, abort_code_upgrade}
    end.
    
That is, they upgrade the state and immediately downgrade it again. If
there is any discrepancy here, then they abort the upgrade. According to
Tanenbaum this captures a remarkable number of botched upgrades.    



https://github.com/gburd/gen_paxos

https://github.com/mattwilliamson/chordial

https://github.com/klarna/tulib

https://github.com/boundary/gen_lb

https://github.com/mochi/statebox

https://github.com/garret-smith/gen_leader_revival

https://github.com/ChicagoBoss/boss_db

https://github.com/kevsmith/gen_nb_server/

https://github.com/basho/riak_dt/

https://erlangcentral.org/wiki/index.php/A_Framework_for_Clustering_Generic_Server_Instances

https://www.fpcomplete.com/blog/2012/06/asynchronous-api-in-c-and-the-continuation-monad

http://amtal.github.io/2011/09/24/monads-in-erlang.html

https://github.com/rabbitmq/erlando





, {'resource_discovery', ".*", {git, "https://github.com/erlware/resource_discovery.git"}}
, {erlcron,              ".*", {git, "https://github.com/erlware/erlcron.git", "master"}}
, {restc, ".*", {git, "git://github.com/kivra/restclient.git", {tag, "0.1.0"}}}

