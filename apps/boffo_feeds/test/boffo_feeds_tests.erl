-module(boffo_feeds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../boffo/include/boffo.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(boffo),
    application:start(boffo_feeds),
    error_logger:tty(false),
    error_logger:logfile({open, "log"}),
    ok.

stop(ok) ->
    boffo_api_feeds:delete_feed("test_feed"),
    error_logger:logfile(close),
    ok.

boffo_feeds_test_ () ->
    [

     {"Test creating a feed",
      ?setup(fun (ok) ->
                     Response = boffo_api_feeds:create_feed("test_feed", test_feeds_callback),
                     ?_assertMatch({ok, _Result}, Response)
             end)},

     {"Test joining an existing feed",
      ?setup(fun (ok) ->
                     boffo_api_feeds:create_feed("test_feed", test_feeds_callback),
                     Response = boffo_api_feeds:join_feed("test_feed", self(), [self()]),
                     ?_assertMatch({ok, _Result}, Response)
             end)},

     {"Test joining a non-existant feed",
      ?setup(fun (ok) ->
                     Response = boffo_api_feeds:join_feed("test_feed", self(), [self()]),
                     ?_assertMatch({error, _Reason}, Response)
             end)},

     {"Test pushing an event on existing feed",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_feeds:create_feed("test_feed", test_feeds_callback),
          Response = boffo_api_feeds:push_event("test_feed", event),
          ?_assertMatch({ok, ok}, Response)
      end)},

     {"Test pushing an event on non-existant feed",
      ?setup(fun (ok) ->
          Response = boffo_api_feeds:push_event("test_feed", event),
          ?_assertMatch({error, _Reason}, Response)
      end)},

     {"Test that the callback module is called on push_event",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_feeds:create_feed("test_feed", test_feed_callback),
          {ok, _} = boffo_api_feeds:join_feed("test_feed", self(), [self()]),
          {ok, ok} = boffo_api_feeds:push_event("test_feed", event),
          Event = receive
                      event -> ok
                   after 1000 ->
                      error
                   end,
          ?_assertEqual(Event, ok)
      end)},

     {"Test leaving a feed",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_feeds:create_feed("test_feed", test_feed_callback),
          {ok, _} = boffo_api_feeds:join_feed("test_feed", self(), [self()]),
          {ok, _} = boffo_api_feeds:leave_feed("test_feed", self()),
          {ok, ok} = boffo_api_feeds:push_event("test_feed", event),
          Event = receive
                      _Msg -> error
                   after 1000 ->
                      ok
                   end,
          ?_assertEqual(Event, ok)
      end)},

     {"Test deleting a feed",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_feeds:create_feed("test_feed", test_feed_callback),
          {ok, _} = boffo_api_feeds:join_feed("test_feed", self(), [self()]),
          fun () ->
                  Del_Result = boffo_api_feeds:delete_feed("test_feed"),
                  ?assertMatch({ok, _}, Del_Result),
                  Push_Result = boffo_api_feeds:push_event("test_feed", event),
                  ?assertMatch({error, _}, Push_Result)
          end
      end)}

    ].
