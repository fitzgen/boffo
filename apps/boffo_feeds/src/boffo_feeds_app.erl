-module(boffo_feeds_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, []).

start(_StartType, _StartArgs) ->
    boffo_util:ensure_mnesia_schema(node()),
    mnesia:start(),

    pg2:create(boffo_feeds_server),
    boffo_feeds_sup:start_link().

stop(_State) ->
    ok.
