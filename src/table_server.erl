

diskless    ram_copies          ram_copy
journal     disk_only_copies    disk_only_copy
standard    disk_copies         disk_copy


-spec tables() -> [atom()]
tables() ->

table_info(table1) ->
    [ FIELDS(table1)   % must be a literal atom
    , INDEXES(field1, field2)
    , disc_copy
    ]


mnesia:create_table(data, [{attributes, record_info(fields, data)},
                               {disc_copies, Nodes},
                               {type, set}]),
                               
    case mnesia:create_table(Tab, TabDef1) of
      {atomic, ok} -> ok;
      {aborted, Reason} ->
          throw({error, {table_creation_failed,
                         Tab, TabDef1, Reason}})
    end
mnesia:create_table(city,   
                        [{ram_copies, [node()|nodes()]},   
                         {attributes, record_info(fields, city)}]),
                         
                               
case mnesia:wait_for_tables(TableNames, Timeout) of
        ok ->
            ok;
        {timeout, BadTabs} ->
            throw({error, {timeout_waiting_for_tables, BadTabs}});
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.                             
    
    
Tables = mnesia:system_info(tables),


mnesia:subscribe({table, account, detailed}),
mnesia:subscribe({table, transaction, detailed}),
    

net_kernel:monitor_nodes(true, [nodedown_reason])
