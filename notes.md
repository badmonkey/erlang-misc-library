


%example: justquick (actor) closed (verb) issue 2 (object) on activity-stream (target) 12 hours ago
%action(Actor, Verb, [Action_object], [Target])


cf push -b https://github.com/spiegela/cf-buildpack-erlang




                              
%net_kernel:monitor_nodes(true, [nodedown_reason])



upgrade(OldVsn, NextVsn, State) ->
    NextState = code_change(NextVsn, State),
    case code_change({down, OldVsn}, NextState) of
        State -> {ok, NextState};
        _ -> {error, abort_code_upgrade}
    end.
    
That is, they upgrade the state and immediately downgrade it again. If
there is any discrepancy here, then they abort the upgrade. According to
Tanenbaum this captures a remarkable number of botched upgrades.    


https://github.com/ErlyORM/boss_db


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

https://github.com/NetComposer/nkdocker

https://github.com/NetComposer/nkdist






, {'resource_discovery', ".*", {git, "https://github.com/erlware/resource_discovery.git"}}
, {erlcron,              ".*", {git, "https://github.com/erlware/erlcron.git", "master"}}
, {restc, ".*", {git, "git://github.com/kivra/restclient.git", {tag, "0.1.0"}}}



chain_apply:first( ok
                 , if_ok_then( somefunc )
                 , if_ok_then(
                        fun() ->
                        end)
                 , if_ok_then() )
                 
and_then:first( expr
              , somefun()
              , fun(input) ->
                end
              , otherfun(1, 2) )

              
              
via module
    register_name/2, unregister_name/1, whereis_name/1 and send/2

make_init_return()    


delay_startup:start_link(Module, Args, Options) -> Result
delay_startup:start_link(ServerName, Module, Args, Options) -> Result


init(Args) ->
    startphase:required_names([srv1, srv2, srv3]),
    
    startphase:init_return(State, DelayInitMessage)
    startphase:init_return({error,E}, DelayInitMessage)
    startphase:continue_init_later()
    startphase:postpone_init()
    

    
    
{ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
 | {stop,Reason} | ignore    
             
{ok,Pid} | ignore | {error,Error}   from start_link             
              
              
              
where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).

name_register({local, Name} = LN) ->
    try register(Name, self()) of
        true -> true
    catch
        error:_ ->
            {false, where(LN)}
    end;
name_register({global, Name} = GN) ->
    case global:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end;
name_register({via, Module, Name} = GN) ->
    case Module:register_name(Name, self()) of
        yes ->
            true;
        no ->
            {false, where(GN)}
    end.
    

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    _ = (catch unregister(Name));
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name);
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.
    
    
    
    
public class Hexbin {
    private static final double thirdPi = Math.PI/3.0;

    private double dx;
    private double dy;

    public Hexbin(double radius) {
        this.dx = 2 * radius * Math.sin(thirdPi);
        this.dy = 1.5 * radius;
    }

    public double[] bin(double x, double y) {
        double py = y/dy;
        double pj = Math.round(py);
        double px = x/dx - ( (int)pj & 1) / 2.0;
        double pi = Math.round(px);
        double py1 = py - pj;

        if (3*Math.abs(py1) > 1) {
            double px1 = px - pi;
            double pi2 = pi + (px < pi ? -1 : 1) / 2.0;
            double pj2 = pj + (py < pj ? -1 : 1);
            double px2 = px - pi2;
            double py2 = py - pj2;

            if (px1*px1 + py1*py1 > px2*px2 + py2*py2) {
                pi = pi2 + ( ( (int)pj & 1) != 0 ? 1 : -1) / 2.0;
                pj = pj2;
            }
        }
        return new double[] {
                (pi + ( (int)pj & 1) / 2.0) * dx,
                pj * dy
        };
    }
}


headers4(Delivery) ->
    catch lists:foldl(fun (_, undefined) -> throw(undefined);
                          (F, Acc) -> F(Acc)
                      end,
                      Delivery,
                      [fun get_msg/1, fun get_content/1,
                       fun get_props/1, fun get_headers/1]).
                       


child_spec includes supervisor entry for anon sups (to help build tree)


[ { supervisor
  , [ {child_specs}
    ]
  }
]


{unique_gen_id, {supevisor, start_link, [super_child, [ChildSpecs]]} }


super_child:init(ChildSpecs) ->
	super_beaviour:build_specs(ChildSpecs).



Module
{supervisor, ChildSpecList}
{supervisor, IdAtom, ChildSpecList}
{Module, Args}
{IdAtom, Module}
{IdAtom, Module, Args}
{IdAtom, Module, Args, Restart, SpecType}
Spec :: tuple()




child_spec() = 
    #{id := child_id(),
      start := mfargs(),
      restart => restart(),
      shutdown => shutdown(),
      type => worker(),
      modules => modules()} |
    {Id :: child_id(),
     StartFunc :: mfargs(),
     Restart :: restart(),
     Shutdown :: shutdown(),
     Type :: worker(),
     Modules :: modules()}

