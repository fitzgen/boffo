-module(boffo_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    pg2:create(boffo_auth_token_server),
    mnesia:start(),
    Token_Server = make_process(boffo_auth_token),
    Passwd_Server = make_process(boffo_auth_passwd),
    {ok, { {one_for_one, 5, 10}, [Token_Server, Passwd_Server]} }.

make_process(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, 500, worker, [Mod]}.
