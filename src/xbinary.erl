
-module(xbinary).

-export([copy_if_large_ref/1, partition/2]).


%%%%% ------------------------------------------------------- %%%%%


copy_if_large_ref(Binary) ->
    case binary:referenced_byte_size(Binary) of
        Large   when Large > 2 * byte_size(Binary)  ->
            binary:copy(Binary)
            
    ;   _       -> Binary
    end.
    

%%%%% ------------------------------------------------------- %%%%%


-spec partition( binary(), integer() ) -> { binary(), binary() }.

partition(Subject, 0) ->
    {<<>>, Subject};
    
    
partition(Subject, Pos)
        when Pos < 0  ->
    Size = byte_size(Subject),
    { binary:part(Subject, Size, Pos)
    , binary:part(Subject, 0, Size + Pos) };

partition(Subject, Pos) ->
    Size = byte_size(Subject),
    { binary:part(Subject, 0, Pos)
    , binary:part(Subject, Pos, Size - Pos) }.

