-module(boffo_util).
-export([ensure_mnesia_scheme/1]).

ensure_mnesia_schema(Node) ->
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.
