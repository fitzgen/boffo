%%-*- mode: erlang -*-
-module(boffo_game_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../boffo/include/boffo.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(boffo),
    application:start(boffo_game),

    error_logger:tty(false),
    error_logger:logfile({open, "log"}),

    {ok, Server} = boffo_game_serv:start_link(),
    pg2:create(test_game_logic),
    {ok, Logic} = test_game_logic:start_link(),
    {Server, Logic}.

stop({Server, Logic}) ->
    error_logger:logfile(close),
    exit(Server, normal),
    pg2:delete(boffo_game_server),
    exit(Logic, normal),
    pg2:delete(test_game_logic).

boffo_game_test_ () ->
    [

     {"Test creating a game",
      ?setup(fun ({_Server, _Logic}) ->
                     Response = boffo_api_game:create_game(test_game_logic,
                                                           sets:from_list(["nick", "matt"])),
                     ?_assertMatch({ok, _Game_Id}, Response)
             end)},

     {"Test getting a game",
      ?setup(fun ({_Server, _Logic}) ->
                     {ok, Game_Id} = boffo_api_game:create_game(test_game_logic,
                                                                sets:from_list(["nick", "matt"])),
                     {ok, Game} = boffo_api_game:get_game(node(), Game_Id),
                     [
                      ?_assertMatch(Game_Id, Game#game.id),
                      ?_assertEqual(sets:from_list(["nick", "matt"]), Game#game.players)
                     ]
             end)},

     {"Test taking a turn",
      [?setup(fun ({_Server, _Logic}) ->
                      {ok, Game_Id} = boffo_api_game:create_game(test_game_logic,
                                                                 sets:from_list(["nick", "matt"])),
                      ok = boffo_api_game:add_turn(node(), Game_Id, "matt", 2),
                      ok = boffo_api_game:add_turn(node(), Game_Id, "nick", 2),
                      {ok, Game} = boffo_api_game:get_game(node(), Game_Id),
                      [
                       ?_assertMatch(Game_Id, Game#game.id),
                       ?_assertEqual(sets:from_list(["nick", "matt"]), Game#game.players),
                       ?_assertMatch(4, Game#game.state),
                       ?_assertMatch(none, Game#game.victor),
                       ?_assertMatch([{"nick", 2},
                                      {"matt", 2}],
                                     Game#game.turns)
                      ]
              end),
       ?setup(fun ({_Server, _Logic}) ->
                      {ok, Game_Id} = boffo_api_game:create_game(test_game_logic,
                                                                 sets:from_list(["nick", "matt"])),
                      ok = boffo_api_game:add_turn(node(), Game_Id, "matt", 100),
                      {ok, Game} = boffo_api_game:get_game(node(), Game_Id),
                      [
                       ?_assertMatch(Game_Id, Game#game.id),
                       ?_assertEqual(sets:from_list(["nick", "matt"]), Game#game.players),
                       ?_assertMatch(100, Game#game.state),
                       ?_assertMatch("matt", Game#game.victor),
                       ?_assertMatch([{"matt", 100}], Game#game.turns)
                      ]
              end)
      ]},

     {"Test chatting",
      ?setup(fun ({_Server, _Logic}) ->
                     {ok, Game_Id} = boffo_api_game:create_game(test_game_logic,
                                                                sets:from_list(["nick", "matt"])),
                     Chats = [
                              {Game_Id, "matt", "Hey"},
                              {Game_Id, "nick", "Sup?"},
                              {Game_Id, "matt", "Chillin'"}
                             ],
                     lists:foreach(fun ({_, User, Msg}) ->
                                           ok = boffo_api_game:add_chat(node(), Game_Id, User, Msg)
                                   end,
                                   Chats),
                     {ok, Game} = boffo_api_game:get_game(node(), Game_Id),
                     lists:map(fun ({{_, Sent_User, Sent_Msg},
                                     #chat{user=Recvd_User, message=Recvd_Msg}}) ->
                                       [
                                        ?_assertMatch(Sent_User, Recvd_User),
                                        ?_assertMatch(Sent_Msg, Recvd_Msg)
                                       ]
                               end,
                               %% Reverse because chats are ordered newest to oldest.
                               lists:zip(Chats, lists:reverse(Game#game.chat)))
             end)}

    ].
