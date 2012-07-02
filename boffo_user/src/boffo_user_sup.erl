
-module(boffo_user_sup).

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
    Status_Server = ?CHILD(boffo_user_status, worker),
    Event_Server = ?CHILD(boffo_user_eventmgr, worker),
    {ok, { {one_for_one, 5, 10}, [Status_Server, Event_Server]} }.
