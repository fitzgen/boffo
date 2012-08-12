%% @author Mochi Media <dev@mochimedia.com>
%% @copyright boffo_frontend Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the boffo_frontend application.

-module(boffo_frontend_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/0, start/2,stop/1]).


start() ->
    start(normal, []).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for boffo_frontend.
start(_Type, _StartArgs) ->
    io:format(user, "starting frontend", []),
    boffo_frontend_deps:ensure(),
    boffo_frontend_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for boffo_frontend.
stop(_State) ->
    ok.
