%%-*- mode: erlang -*-
-module(boffo_api_game).
-export([create_game/2, get_game/2, add_turn/4, add_chat/4]).

-include_lib("boffo/include/boffo.hrl").

%% @doc Creates a new game of the type of the corresponding game logic process
%%      group.
-spec create_game(term(), set()) -> {ok, boffo_game_id()} | {error, any()}.
create_game(Game_Logic_PG, Players) ->
    Server = pg2:get_closest_pid(boffo_game_serv),
    gen_server:call(Server, {create, Game_Logic_PG, Players}).

%% @doc Retrieve the game whose id is Game_Id.
-spec get_game(node(), boffo_game_id()) -> {ok, #game{}} | {error, any()}.
get_game(Node, Game_Id) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {get, Game_Id}).

%% @doc Add a turn to the game whose id is Game_id.
-spec add_turn(node(), boffo_game_id(), string(), any()) -> ok | {error, any()}.
add_turn(Node, Game_Id, User, Turn) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {turn, {Game_Id, User, Turn}}).

%% @doc Add a message to the chat log for the game whose id is Game_Id.
-spec add_chat(node(), boffo_game_id(), string(), string()) -> ok | {error, any()}.
add_chat(Node, Game_Id, User, Msg) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {chat, {Game_Id, User, Msg}}).
