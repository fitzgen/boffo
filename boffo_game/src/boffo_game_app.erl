-module(boffo_game_app).

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

    pg2:create(boffo_game_server),
    pg2:create(boffo_game_logic_server),
    % pg2:create(boffo_game_event),

    boffo_game_sup:start_link().

stop(_State) ->
    ok.
