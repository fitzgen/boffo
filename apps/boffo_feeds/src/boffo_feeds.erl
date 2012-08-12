-module(boffo_feeds).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(feed, {name, event_pid, callback_mod}).
-record(subscription, {handler_id, feed_name, subscriber_pid}).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    pg2:join(boffo_feeds_server, Pid),
    {ok, Pid}.

%% Initialization

table_spec(Fields) ->
    [{type, ordered_set}, {disc_copies, [node()]}, {attributes, Fields}].

ensure_tables() ->
    mnesia:create_table(feed, table_spec(record_info(fields, feed))),
    mnesia:create_table(subscription, table_spec(record_info(fields, subscription))).

init(_Options) ->
    ensure_tables(),
    {ok, []}.

%% Server API

handle_call({create_feed, Name, Callback_Mod}, _From, State) ->
    {reply, create_feed(Name, Callback_Mod), State};

handle_call({delete_feed, Name}, _From, State) ->
    {reply, delete_feed(Name), State};

handle_call({join_feed, Name, My_Pid, Resp}, _From, State) ->
    {reply, join_feed(Name, My_Pid, Resp), State};

handle_call({leave_feed, Name, My_Pid}, _From, State) ->
    {reply, leave_feed(Name, My_Pid), State};

handle_call({push_event, Name, Event}, _From, State) ->
    {reply, push_event(Name, Event), State}.

%% Implementation

create_feed(Name, Callback_Mod) ->
    delete_feed(Name),
    {ok, Pid} = gen_event:start_link(),
    write_feed(Name, Pid, Callback_Mod).

join_feed(Name, My_Pid, Resp) ->
    case get_feed(Name) of
        {ok, Feed} ->
            Callback_Mod = Feed#feed.callback_mod,
            Handler_Id = {Callback_Mod, make_ref()},
            gen_event:add_sup_handler(Feed#feed.event_pid, Handler_Id, [Resp]),
            write_subscription(Name, My_Pid, Handler_Id);
        {error, Reason} ->
            {error, Reason}
    end.

leave_feed(Name, My_Pid) ->
    leave_this_feed(get_feed(Name), get_subscription(Name, My_Pid)).

leave_this_feed({ok, Feed}, {ok, Subscription}) ->
    Event_Pid = Feed#feed.event_pid,
    Handler_Id = Subscription#subscription.handler_id,
    gen_event:delete_handler(Event_Pid, Handler_Id, []),
    delete_subscription(Handler_Id);
leave_this_feed({error, Reason1}, {error, Reason2}) ->
    {error, strings:join("; ", [Reason1, Reason2])};
leave_this_feed({error, Reason}, _) ->
    {error, Reason};
leave_this_feed(_, {error, Reason}) ->
    {error, Reason}.

push_event(Name, Event) ->
    case get_feed(Name) of
        {ok, Feed} ->
            gen_event:notify(Feed#feed.event_pid, Event),
            {ok, ok};
        {error, Reason} ->
            {error, Reason}
    end.


%% mnesia calls
transaction(Fn) ->
    case mnesia:transaction(Fn) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

single_result(TRes) ->
    case TRes of
        {ok, [Single]} ->
            {ok, Single};
        {ok, []} ->
            {error, no_result};
        {error, Reason} ->
            {error, Reason}
    end.

write_feed(Name, Pid, Callback_Mod) ->
    T = fun() ->
                mnesia:write(#feed{name=Name,
                                   event_pid=Pid,
                                   callback_mod=Callback_Mod})
        end,
    transaction(T).

delete_feed(Name) ->
    T = fun() ->
                mnesia:delete({feed, Name})
        end,
    transaction(T).

get_feed(Name) ->
    T = fun() ->
                mnesia:read({feed, Name})
        end,
    single_result(transaction(T)).

write_subscription(Name, My_Pid, Handler_Id) ->
    Subscr = #subscription{feed_name=Name,
                           subscriber_pid=My_Pid,
                           handler_id=Handler_Id},
    T = fun() ->
                mnesia:write(Subscr)
        end,
    transaction(T).

delete_subscription(Handler_Id) ->
    T = fun() ->
                mnesia:delete({subscription, Handler_Id})
        end,
    transaction(T).

get_subscription(Feed_Name, My_Pid) ->
    T = fun() ->
                Q = qlc:q([X || X <- mnesia:table(subscription),
                                X#subscription.feed_name =:= Feed_Name,
                                X#subscription.subscriber_pid =:= My_Pid]),
                qlc:e(Q)
        end,
    single_result(transaction(T)).

%% etc.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
