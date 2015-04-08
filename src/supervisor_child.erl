
-module(supervisor_child).


-callback child_spec(SpecId :: atom(), Args :: list()) -> supervisor:child_spec().

