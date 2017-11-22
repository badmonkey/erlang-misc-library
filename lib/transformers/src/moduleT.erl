
-module(moduleT).


-record( definition
       , { name                     :: atom()
         , func_order               :: [ functionT:id() ]
         , func_export              :: sets:set( functionT:id() )
         , func_defns               :: #{ functionT:id() => functionT:definition() }
         }).
-type definition() :: #definition{}.


-spec create() -> definition().
-spec create_from( Module :: atom() | stdforms:formlist() ) -> definition().

-spec to_forms( definition() ) -> stdforms:formlist().
         

-spec function( definition(), functionT:id() ) -> functionT:definition().

-spec function_scope( definition(), functionT:id() ) -> functionT:scope().
-spec function_scope( definition(), functionT:id(), functionT:scope() ) -> definition().

-spec function_remove( definition(), functionT:id() ) -> definition().
-spec function_update( definition(), functionT:id(), functionT:definition() ) -> definition().
-spec function_append( definition(), functionT:id(), functionT:definition() ) -> definition().



%
% parse_transform(ParseTree, _Options) ->
%   Defn = moduleT:create_from(ParseTree),
%   ...
%   moduleT:to_forms(Defn).
%