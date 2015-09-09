
-module(xbinary).
-extends(binary).


-export([copy_if_large_ref/1]).


%%%%% ------------------------------------------------------- %%%%%


copy_if_large_ref(Binary) ->
	case binary:referenced_byte_size(Binary) of
		Large	when Large > 2 * byte_size(Binary)  ->
			binary:copy(Binary)
			
	;	_ 		-> Binary
	end.
	
	

