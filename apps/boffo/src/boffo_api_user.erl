-module(boffo_api_user).

-export([set_online/2, get_online/1]).
-export([add_game/2, remove_game/2, get_games/1, clear_games/1]).

%% Online Status

%% @spec set_online(str(), boolean()) -> {ok, term()} | {error, term()}
set_online(Username, Value) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {set_online, Username, Value}).

%% @spec get_online(str()) -> {ok, boolean()} | {error, term()}
get_online(Username) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {get_online, Username}).

%% Game Management

%% @spec add_game(str(), term()) -> {ok, term()} | {error, term()}
add_game(Username, Game_Id) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {add_game, Username, Game_Id}).

remove_game(Username, Game_Id) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {remove_game, Username, Game_Id}).

get_games(Username) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {get_games, Username}).

clear_games(Username) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {clear_games, Username}).
