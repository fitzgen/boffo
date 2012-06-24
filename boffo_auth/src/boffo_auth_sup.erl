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
    Token_Server = {boffo_auth_token,
                    {boffo_auth_token, start_link, []},
                    transient,
                    500,
                    worker,
                    [boffo_auth_token]},
    %% TODO: start passwd server
    {ok, { {one_for_one, 5, 10}, [Token_Server]} }.
