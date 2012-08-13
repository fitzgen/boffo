%%-*- mode: erlang -*-
-module(boffo_mgr_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../boffo/include/boffo.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    application:start(boffo),
    application:start(boffo_mgr),
    {ok, Server} = boffo_mgr_serv:start_link(),
    Id = uuid:v4(),
    ok = boffo_api_mgr:set_node(Id, node()),
    {Id, Server}.

stop({_Id, Server}) ->
    exit(Server, normal).

boffo_game_test_ () ->
    [

     {"Test setting a node that exists",
      ?setup(fun ({Id, _Server}) ->
                     Response = boffo_api_mgr:set_node(Id, foo@bar),
                     fun () ->
                             ?assertMatch(Response, ok),
                             Response1 = boffo_api_mgr:get_node(Id),
                             ?assertMatch(Response1, {ok, foo@bar})
                     end
             end)},

     {"Test setting a node that does not exists",
      ?setup(fun ({_Id, _Server}) ->
                     New_Id = uuid:v4(),
                     Response = boffo_api_mgr:set_node(New_Id, foo@bar),
                     fun () ->
                             ?assertMatch(Response, ok),
                             Response1 = boffo_api_mgr:get_node(New_Id),
                             ?assertMatch(Response1, {ok, foo@bar})
                     end
             end)},

     {"Test getting a node",
      ?setup(fun ({Id, _Server}) ->
                     Response = boffo_api_mgr:get_node(Id),
                     Expected = {ok, node()},
                     ?_assertMatch(Expected, Response)
             end)}

    ].
