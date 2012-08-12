-module(boffo_frontend_feed).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

init([Resp]) ->
    {ok, Resp}.

handle_event(Event, Resp) ->
    %% My_Pid should be a boffo_frontend... use gen_server:call ??
    Request = Resp:get(request),
    io:format("should close? ~p~n", [Request:get_header_value("connection")]),
    case Request:should_close() of
        true ->
            remove_handler;
        false ->
            Resp:write_chunk(Event),
            {ok, Resp}
    end.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
