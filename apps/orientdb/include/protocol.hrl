

-define(ODB_PROTOCOL_VER, 1).



-record(column,    {name, type, size, modifier, format}).
-record(statement, {name, columns, types}).

-record(error,  {severity, code, message, extra}).

-define(O_DRV_NAME, <<"OrientDB Erlang Client">>).
-define(O_DRV_VER, <<"0.1">>).
-define(O_PROTO_VER, 1).

-define(O_SHUTDOWN, 1).
-define(O_CONNECT, 2).
-define(O_DB_OPEN, 3).
-define(O_DB_CREATE, 4).
-define(O_DB_CLOSE, 5).
-define(O_DB_EXIST, 6).
-define(O_DB_DELETE, 7).
-define(O_DB_SIZE, 8).
-define(O_DB_COUNTRECORDS, 9).
-define(O_DATACLUSTER_ADD, 10).
-define(O_DATACLUSTER_REMOVE, 11).
-define(O_DATACLUSTER_COUNT, 12).
-define(O_DATACLUSTER_DATARANGE, 13).
-define(O_DATACLUSTER_COPY, 14).
-define(O_DATACLUSTER_LH_CLUSTER_IS_USED, 16).
-define(O_DATASEGMENT_ADD, 20).
-define(O_DATASEGMENT_REMOVE, 21).
-define(O_REQUEST_RECORD_METADATA, 29).
-define(O_RECORD_LOAD, 30).
-define(O_RECORD_CREATE, 31).
-define(O_RECORD_UPDATE, 32).
-define(O_RECORD_DELETE, 33).
-define(O_RECORD_COPY, 34).
-define(O_REQUEST_RECORD_CHANGE_IDENTITY, 35).
-define(O_REQUEST_POSITIONS_HIGHER, 36).
-define(O_REQUEST_POSITIONS_LOWER, 37).
-define(O_REQUEST_RECORD_CLEAN_OUT, 38).
-define(O_REQUEST_POSITIONS_FLOOR, 39).
-define(O_COMMAND, 41).
-define(O_REQUEST_POSITIONS_CEILING, 42).
-define(O_TX_COMMIT, 60).
-define(O_CONFIG_GET, 70).
-define(O_CONFIG_SET, 71).
-define(O_CONFIG_LIST, 72).
-define(O_DB_RELOAD, 73).
-define(O_DB_LIST, 74).
-define(O_REQUEST_PUSH_RECORD, 74).
-define(O_REQUEST_PUSH_DISTRIB_CONFIG, 74).
-define(O_REQUEST_DB_COPY, 74).
-define(O_REQUEST_REPLICATION, 74).
-define(O_REQUEST_CLUSTER, 74).
-define(O_REQUEST_DB_TRANSFER, 74).
-define(O_REQUEST_DB_FREEZE, 74).
-define(O_REQUEST_DB_RELEASE, 74).
-define(O_OK, 1).
-define(O_ERROR, 0).

