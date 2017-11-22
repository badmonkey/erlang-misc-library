
-module(functionT).      
   

-type id() :: { atom(), non_neg_integer() }.
-type remore_id() :: { atom(), atom(), non_neg_integer() }.
-type scope() :: public | private.

-record( definition
       , { data
         }).
-type definition() :: #definition{}.
    