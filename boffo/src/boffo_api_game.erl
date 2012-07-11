-module(boffo_api_game).
-export([create_game/2, get_game/2, add_turn/4, add_chat/4]).

create_game(Game_Logic_PG, Players) ->
    Server = pg2:get_closest_pid(boffo_game_serv),
    gen_server:call(Server, {create, Game_Logic_PG, Players}).

get_game(Node, Game_Id) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {get, Game_Id}).

add_turn(Node, Game_Id, User, Turn) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {turn, {Game_Id, User, Turn}}).

add_chat(Node, Game_Id, User, Msg) ->
    Server = pg2:get_closest_pid({Node, boffo_game_serv}),
    gen_server:call(Server, {chat, {Game_Id, User, Msg}}).
