-module(boffo_api_feeds).

-export([create_feed/2, delete_feed/1, join_feed/3, leave_feed/2, push_event/2]).

%% @spec create_feed(term(), mod()) -> {ok, term()} | {error, term()}
create_feed(Name, Callback_Mod) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {create_feed, Name, Callback_Mod}).

%% @spec delete_feed(term()) -> {ok, term()} | {error, term()}
delete_feed(Name) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {delete_feed, Name}).

%% @spec join_feed(term(), pid(), [term()]) -> {ok, term()} | {error, term()}
join_feed(Name, My_Pid, Args) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {join_feed, Name, My_Pid, Args}).

%% @spec leave_feed(term(), pid()) -> {ok, term()}, | {error, term()}
leave_feed(Name, My_Pid) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {leave_feed, Name, My_Pid}).

%% @spec push_event(term(), term()) -> {ok, ok} | {error, term()}
push_event(Name, Event) ->
    Server = pg2:get_closest_pid(boffo_feeds_server),
    gen_server:call(Server, {push_event, Name, Event}).
