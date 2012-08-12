%%-*- mode: erlang -*-
-module(boffo_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {{I, make_ref()}, {I, start_link, []}, permanent, 5000, Type, [I]}).

-include("$BOFFO_SETTINGS").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Token_Server = ?CHILD(boffo_auth_token, worker),
    Passwd_Servers = [?CHILD(boffo_auth_passwd, worker) || _ <- lists:seq(1, ?BOFFO_NUM_PASSWD_SERVERS)],
    {ok, { {one_for_one, 5, 10}, lists:flatten([Token_Server, Passwd_Servers])} }.
