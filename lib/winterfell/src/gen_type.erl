
-module(gen_type).


%%%%% ------------------------------------------------------- %%%%%


-opaque unique_tag() :: any().

-type from() :: {pid(), unique_tag()}.
-type timeout_type() :: hibernate | non_neg_integer().
-type reason() :: normal | shutdown | {shutdown, term()} | term().
-type version() :: term() | {down, term()}.


%%%%% ------------------------------------------------------- %%%%%


-type ok_state(T) :: {ok, T} | {ok, T, timeout_type()}.
-type stateless_stop() :: {stop, reason()}.
                                         
-type stop(T) :: {stop, reason(), T}.

-type stop_reply(Rply, T) :: {stop, reason(), Rply, T} | stop(T).
-type stop_reply(T) :: stop_reply(_, T).

-type reply(Rply, T) :: {reply, Rply, T} | {reply, Rply, T, timeout_type()}.
-type reply(T) :: reply(_, T).

-type noreply(T) :: {noreply, T} | {noreply, T, timeout_type()}.

-type reply_send(Rply, Send, T) :: {reply_send, Rply, Send, T} | {reply_send, Rply, Send, T, timeout_type()}.
-type reply_send(T) :: reply_send(_, _, T).

-type noreply_send(Send, T) :: {noreply_send, Send, T} | {noreply_send, Send, T, timeout_type()}.
-type noreply_send(T) :: noreply_send(_, T).

    
%%%%% ------------------------------------------------------- %%%%%
    

-type init_result(T) :: ok_state(T)
                      | ignore | hibernate
                      | stateless_stop().

-type call_result(Rply, T) :: reply(Rply, T) | noreply(T) | stop_reply(Rply, T).
-type call_result(T) :: call_result(_, T).
-type cast_result(T) :: noreply(T) | stop(T).
-type info_result(T) :: noreply(T) | stop(T).

-type terminate_result() :: term().
-type code_change_result(T) :: {ok, T} | {error, reason()}.


-type send_result(Rply, Send ,T) :: reply_send(Rply, Send, T) | noreply_send(Send, T).
-type send_result(T) :: send_result(_, _, T).


%%%%% ------------------------------------------------------- %%%%%


-type init_callback(T) :: fun( (Args :: term()) -> init_result(T) ).

-type handle_call_callback(Reqst, Rply, T) :: fun( (Reqst, from(), T) -> call_result(Rply, T) ).
-type handle_call_callback(T) :: handle_call_callback(term(), term(), T).

-type handle_cast_callback(Reqst, T) :: fun( (Reqst, T) -> cast_result(T) ).
-type handle_cast_callback(T) :: handle_cast_callback(term(), T).

-type handle_info_callback(Reqst, T) :: fun( (Reqst, T) -> info_result(T) ).
-type handle_info_callback(T) :: handle_info_callback(term(), T).
    
-type terminate_callback(T) :: fun( (reason(), T) -> terminate_result() ).

-type code_change_callback(Xtra, T) :: fun( (version(), T, Xtra) -> code_change_result(T) ).
-type code_change_callback(T) :: code_change_callback(term(), T).


-type gen_service(ReqstCall, ReplyCall, ReqstCast, ReqstInfo, T) ::
        { gen_service
        , init_callback(State)
        , handle_call_callback(ReqstCall, ReplyCall, T)
        , handle_cast_callback(ReqstCast, T)
        , handle_info_callback(ReqstInfo, T)
        , terminate_callback(State)
        , code_change_callback(T)
        }.


%%%%% ------------------------------------------------------- %%%%%