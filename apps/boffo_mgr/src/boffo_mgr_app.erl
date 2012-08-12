%%-*- mode: erlang -*-
-module(boffo_mgr_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, []).

start(_StartType, _StartArgs) ->
    boffo_mgr_sup:start_link().

stop(_State) ->
    ok.
