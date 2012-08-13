%%-*- mode: erlang -*-
-module(boffo_api_mgr).
-export([get_node/1, set_node/2]).

-include_lib("boffo/include/boffo.hrl").

%% @doc Returns the node that the game data for the game whose ID is Game_Id
%%      lives on.
-spec get_node(boffo_game_id()) -> {ok, atom()} | {error, any()}.
get_node(Game_Id) ->
    Server = pg2:get_closest_pid(boffo_mgr_server),
    gen_server:call(Server, {get_node, Game_Id}).

%% @doc Retrieve the game whose id is Game_Id.
-spec set_node(boffo_game_id(), atom()) -> ok | {error, any()}.
set_node(Game_Id, Node) ->
    Server = pg2:get_closest_pid(boffo_mgr_server),
    gen_server:call(Server, {set_node, Game_Id, Node}).
