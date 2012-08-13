-module(boffo_user_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../boffo/include/boffo.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(test_user, "test_user").
-define(test_game1, "test_game1").
-define(test_game2, "test_game2").

start() ->
    application:start(boffo),
    application:start(boffo_user),
%    error_logger:tty(false),
%    error_logger:logfile({open, "log"}),
    ok.

stop(ok) ->
%    boffo_api_user:clear_games(?test_user),

    application:stop(boffo),
    application:stop(boffo_user),
%    error_logger:logfile(close),
    ok.

boffo_feeds_test_ () ->
    [

     {"Test set_online return value",
      ?setup(fun (ok) ->
          Response = boffo_api_user:set_online(?test_user, false),
          ?_assertMatch({ok, _}, Response)
      end)},

     {"Test get_online return value",
      ?setup(fun (ok) ->
          fun () ->
              Response = boffo_api_user:get_online(?test_user),
              ?assertMatch({ok, _}, Response),
              {ok, Value} = Response,
              ?assert(is_boolean(Value))
          end
      end)},

     {"Test consistency of set/get online",
      ?setup(fun (ok) ->
          fun () ->
              boffo_api_user:set_online(?test_user, true),
              Response_True = boffo_api_user:get_online(?test_user),
              ?assertMatch({ok, true}, Response_True),
              boffo_api_user:set_online(?test_user, false),
              Response_False = boffo_api_user:get_online(?test_user),
              ?assertMatch({ok, false}, Response_False)
          end
      end)},

     {"Test return value of add_game",
      ?setup(fun (ok) ->
          Response = boffo_api_user:add_game(?test_user, ?test_game1),
          ?_assertEqual({ok, ok}, Response)
      end)},

     {"Test return value of remove_game",
      ?setup(fun (ok) ->
          Response = boffo_api_user:remove_game(?test_user, ?test_game1),
          ?_assertMatch({ok, _}, Response)
      end)},

     {"Test return value of get_games",
      ?setup(fun (ok) ->
          Response = boffo_api_user:get_games(?test_user),
          ?_assertMatch({ok, _}, Response)
      end)},

     {"Test return value of clear_games",
      ?setup(fun (ok) ->
          Response = boffo_api_user:clear_games(?test_user),
          ?_assertMatch({ok, _}, Response)
      end)},

     {"Test add_game consistency",
      ?setup(fun (ok) ->
          {ok, Games_Before} = boffo_api_user:get_games(?test_user),
          {ok, _} = boffo_api_user:add_game(?test_user, ?test_game1),
          {ok, Games_After} = boffo_api_user:get_games(?test_user),
          In_Set = {sets:is_element(?test_game1, Games_Before),
                    sets:is_element(?test_game1, Games_After)},
          ?_assertMatch({false, true}, In_Set)
      end)},

     {"Test remove_game consistency",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_user:add_game(?test_user, ?test_game1),
          {ok, Games_Before} = boffo_api_user:get_games(?test_user),
          {ok, _} = boffo_api_user:remove_game(?test_user, ?test_game1),
          {ok, Games_After} = boffo_api_user:get_games(?test_user),
          In_Set = {sets:is_element(?test_game1, Games_Before),
                    sets:is_element(?test_game1, Games_After)},
          ?_assertMatch({true, false}, In_Set)
      end)},

     {"Test clear_games consistency",
      ?setup(fun (ok) ->
          {ok, _} = boffo_api_user:add_game(?test_user, ?test_game1),
          {ok, _} = boffo_api_user:add_game(?test_user, ?test_game2),
          {ok, _} = boffo_api_user:clear_games(?test_user),
          {ok, Games} = boffo_api_user:get_games(?test_user),
          ?_assertEqual(sets:new(), Games)
      end)}

    ].
