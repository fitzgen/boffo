-module(boffo_api_feeds).

-export([create_feed/2, delete_feed/1, join_feed/3, leave_feed/2, push_event/2]).

create_feed(Name, Callback_Mod) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {create_feed, Name, Callback_Mod}).

delete_feed(Name) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {delete_feed, Name}).

join_feed(Name, My_Pid, Resp) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {join_feed, Name, My_Pid, Resp}).

leave_feed(Name, My_Pid) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {leave_feed, Name, My_Pid}).

push_event(Name, Event) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {push_event, Name, Event}).
