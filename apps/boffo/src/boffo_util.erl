-module(boffo_util).
-export([ensure_mnesia_schema/1, transaction/1, single_result/1, log/1]).

ensure_mnesia_schema(Node) ->
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

transaction(Fn) ->
    case mnesia:transaction(Fn) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

single_result(TRes) ->
    case TRes of
        {ok, [Single]} ->
            {ok, Single};
        {ok, []} ->
            {error, no_result};
        {error, Reason} ->
            {error, Reason}
    end.

log(Args) ->
    case is_list(Args) of
        true ->
            Fmt = string:concat(string:copies("~p ", length(Args)), "~n"),
            io:format(user, Fmt, Args);
        false ->
            io:format(user, "~p~n", [Args])
    end.
