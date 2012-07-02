-module(boffo_game_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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
    pg2:create(boffo_game_server),
    Game_Servers = [CHILD(boffo_game_serv, worker) || _ <- lists:seq(1, ?BOFFO_NUM_GAME_SERVERS) ],
    Event_Server = [],       %% TODO
    Game_Logic_Servers = [], %% TODO
    Children = [Game_Servers, Event_Server, Game_Logic_Servers],
    {ok, { {one_for_one, 5, 10}, lists:flatten(Children)} }.
