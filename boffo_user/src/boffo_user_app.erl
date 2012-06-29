-module(boffo_user_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, []).

start(_StartType, _StartArgs) ->
    ensure_schema(),
    mnesia:start(),

    pg2:create(boffo_user_status_server),
    
    boffo_user_sup:start_link().

stop(_State) ->
    ok.

ensure_schema() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
	ok ->
	    ok;
	{error, {Node, {already_exists, Node}}} ->
	    ok;
	{error, Error} ->
	    {error, Error}
    end.
