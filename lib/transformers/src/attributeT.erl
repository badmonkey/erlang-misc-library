
-module(attributeT).      
   

-type id() :: atom().

-record( definition
       , { data
         }).
-type definition() :: #definition{}.


%
% {attribute, LINE, TYPE, DATA}
%
% export, import, module, file, spec, callback, record, type, opaque
% compile, vsn, on_load
% wild*
%