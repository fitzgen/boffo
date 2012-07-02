-module(boffo_user_eventmgr).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% Public API

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    pg2:join(boffo_user_eventmgr_server, Pid),
    {ok, Pid}.

init(_Options) ->
    {ok, dict:new()}.

handle_call({create_user_feed, Username}, _From, State) ->
    %% possibly use start_link/1
    {ok, Pid} = gen_event:start_link(),
    Next_State = dict:store(Username, Pid, State),
    {reply, {ok, Pid}, Next_State};

handle_call({delete_user_feed, Username}, _From, State) ->
    Next_State = dict:erase(Username, State),
    {reply, {ok, ok}, Next_State};

handle_call({join_feed, Username, My_Pid}, _From, State) ->
    case dict:find(Username, State) of
        {ok, Event_Pid} ->
            Handler_Id = {boffo_user_feed, make_ref()},
            gen_event:add_sup_handler(Event_Pid, Handler_Id, [My_Pid]),
            {reply, {ok, Handler_Id}, State};
        error ->
            {reply, {error, error}, State}
    end;

handle_call({leave_feed, Username, Handler_Id}, _From, State) ->
    case dict:find(Username, State) of
        {ok, Event_Pid} ->
            gen_event:delete_handler(Event_Pid, Handler_Id),
            {reply, {ok, ok}, State};
        error ->
            {reply, {error, error}, State}
    end;

handle_call({push_feed, Username, Event}, _From, State) ->
    case dict:find(Username, State) of
        {ok, Event_Pid} ->
            gen_event:notify(Event_Pid, Event),
            {reply, {ok, ok}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
