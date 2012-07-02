-module(boffo_user_eventmgr_tests).
-include_lib("eunit/include/eunit.hrl").

-define(USER, "matt").

basic_feed_test_() ->
    {setup,
     fun basic_setup/0,
     fun(Pid) ->
         [fun() ->
             {ok, Feed_Pid} = gen_server:call(Pid, {create_user_feed, ?USER}),
             ?assert(is_pid(Feed_Pid))
          end,
          fun() ->
              {ok, Handler_Id} = gen_server:call(Pid, {join_feed, ?USER, self()}),
              {boffo_user_feed, Ref} = Handler_Id,
              ?assert(is_reference(Ref))
          end,
          fun() ->
              ok
          ]
     end
    }.

basic_setup() ->
    io:format(user, "in setup~n", []),
    {ok, Pid} = boffo_user_eventmgr:start_link(),
    Pid.
