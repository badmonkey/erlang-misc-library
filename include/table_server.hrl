
%%%%% ------------------------------------------------------- %%%%%

%
% before using FIELDS make sure the module is compiled with
% -compile([{parse_transform, record_info_runtime}]).
%

-define(FIELDS(T), {attributes, record_info_fields(T)}).


-define(INDEX(F), {index, [F]}).
-define(INDEX(F1, F2), {index, [F1, F2]}).
-define(INDEX(F1, F2, F3), {index, [F1, F2, F3]}).
-define(INDEX(F1, F2, F3, F4), {index, [F1, F2, F3, F4]}).
-define(INDEX(F1, F2, F3, F4, F5), {index, [F1, F2, F3, F4, F5]}).


-define(SET_TYPE, {type, set}).
-define(BAG_TYPE, {type, bag}).


-define(READ_ONLY, {access_mode, read_only}).


-define(RAM_TABLE, {ram_copies, [node()|nodes()]}).
-define(JOURNAL, {disc_only_copies, [node()|nodes()]}).
-define(TABLEDB, {disc_copies, [node()|nodes()]}).


% @todo needs some missing services
-define(CLUSTER_SETUP, {ram_copies, [node()]}, {disc_only_copies, []}, {disc_copies, [nodes()]}).

