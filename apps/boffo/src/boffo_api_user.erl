-module(boffo_api_user).

-export([set_online/2, get_online/1]).
-export([add_game/2, remove_game/2, get_games/1, get_users/1]).
-export([create_feed/1, delete_feed/1, join_feed/2, leave_feed/2, push_feed/2]).

%% Online Status

set_online(Username, Value) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {set_online, Username, Value}).

get_online(Username) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {get_online, Username}).

%% Game Management

add_game(Username, Game_Id) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {add_game, Username, Game_Id}).

remove_game(Username, Game_Id) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {remove_game, Username, Game_Id}).

get_games(Username) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {get_games, Username}).

get_users(Game_Id) ->
    Server = pg2:get_closest_pid(boffo_user_status_server),
    gen_server:call(Server, {get_users, Game_Id}).

%% Event Feeds

create_feed(Username) ->
    Server = pg2:get_closest_pid(boffo_user_eventmgr_server),
    gen_server:call(Server, {create_user_feed, Username}).

delete_feed(Username) ->
    Server = pg2:get_closest_pid(boffo_user_eventmgr_server),
    gen_server:call(Server, {delete_user_feed, Username}).

join_feed(Username, My_Pid) ->
    Server = pg2:get_closest_pid(boffo_user_eventmgr_server),
    gen_server:call(Server, {join_feed, Username, My_Pid}).

leave_feed(Username, Handler_Id) ->
    Server = pg2:get_closest_pid(boffo_user_eventmgr_server),
    gen_server:call(Server, {leave_feed, Username, Handler_Id}).

push_feed(Username, Event) ->
    Server = pg2:get_closest_pid(boffo_user_eventmgr_server),
    gen_server:call(Server, {push_feed, Username, Event}).
