%%-*- mode: erlang -*-
%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc boffo_frontend.

-module(boffo_frontend).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the boffo_frontend server.
start() ->
    io:format("boffo_frontend.erl"),
    boffo_frontend_deps:ensure(),
    ensure_started(crypto),
    application:start(boffo_frontend).


%% @spec stop() -> ok
%% @doc Stop the boffo_frontend server.
stop() ->
    application:stop(boffo_frontend).
