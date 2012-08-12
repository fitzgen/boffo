-module(boffo_user_status).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-compile(export_all).

-include("$BOFFO_SETTINGS").

-record(user_status, {username, is_online, games}).
-record(game_users, {game_id, users}).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    pg2:join(boffo_user_status_server, Pid),
    {ok, Pid}.

handle_call({set_online, Username, Value}, _From, State) ->
    Result = write_rec(#user_status{username=Username, is_online=Value}),
    {reply, Result, State};

handle_call({get_online, Username}, _From, State) ->
    case get_status(Username) of
        {ok, Status} ->
            {reply, {ok, Status#user_status.is_online}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_games, Username}, _From, State) ->
    case get_status(Username) of
        {ok, Status} ->
            Games = Status#user_status.games,
            {reply, {ok, Games}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_users, Username}, _From, State) ->
    case get_game_users(Username) of
        {ok, Game_Users} ->
            Users = list_or_empty(Game_Users#game_users.users),
            {reply, {ok, Users}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({add_game, Username, Game_Id}, _From, State) ->
    Add_Game = fun(Status, Game_Users) ->
        New_Games = sets:add_element(Game_Id, Status#user_status.games),
        New_Users = sets:add_element(Username, Game_Users#game_users.users),
        Write_Both = fun() ->
            mnesia:write(Status#user_status{games=New_Games}),
            mnesia:write(Game_Users#game_users{users=New_Users})
        end,
        transaction(Write_Both)
    end,
    Result = with_status_users(Username, Game_Id, Add_Game),
    {reply, Result, State};

handle_call({remove_game, Username, Game_Id}, _From, State) ->
    Remove_Game = fun(Status, Game_Users) ->
        New_Games = sets:del_element(Game_Id, Status#user_status.games),
        New_Users = sets:del_element(Username, Game_Users#game_users.users),
        Write_Both = fun() ->
            mnesia:write(Status#user_status{games=New_Games}),
            mnesia:write(Game_Users#game_users{users=New_Users})
        end,
        transaction(Write_Both)
    end,
    Result = with_status_users(Username, Game_Id, Remove_Game),
    {reply, Result, State}.

with_status_users(Username, Game_Id, Fn) ->
    Status_Res = get_status(Username),
    Users_Res = get_game_users(Game_Id),
    handle_status_users_results(Status_Res, Users_Res, Fn).

handle_status_users_results({ok, Status}, {ok, Game_Users}, Fn) ->
    Fn(Status, Game_Users);
handle_status_users_results({error, Reason}, {ok, _}, _) ->
    {error, Reason};
handle_status_users_results({ok, _}, {error, Reason}, _) ->
    {error, Reason};
handle_status_users_results({error, Reason1}, {error, Reason2}, _) ->
    {error, string:join([Reason1, Reason2], "; ")}.

list_or_empty(undefined) -> [];
list_or_empty(Lst) -> Lst.

transaction(Fn) ->
    case mnesia:transaction(Fn) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

write_rec(Rec) ->
    Write_Rec = fun() ->
        mnesia:write(Rec)
    end,
    transaction(Write_Rec).

new_status(Username) ->
    #user_status{username=Username, is_online=false, games=sets:new()}.

new_games(Game_Id) ->
    #game_users{game_id=Game_Id, users=sets:new()}.

get_status(Username) ->
    case mnesia:transaction(fun() -> mnesia:read(user_status, Username) end) of
        {atomic, [Status]} ->
            {ok, Status};
        {atomic, []} ->
            {ok, new_status(Username)};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_game_users(Game_Id) ->
    case mnesia:transaction(fun() -> mnesia:read(game_users, Game_Id) end) of
        {atomic, [Game_Users]} ->
            {ok, Game_Users};
        {atomic, []} ->
            {ok, new_games(Game_Id)};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_opponent_usernames(Username) ->
    case get_status(Username) of
        {ok, Status} ->
            Games_List = sets:to_list(Status#user_status.games),
            Games_Users_List_Fn = fun() ->
                Q = qlc:q([X#game_users.users || X <- mnesia:table(game_users),
                                                 lists:member(X#game_users.game_id, Games_List)]),
                qlc:e(Q)
            end,
            case transaction(Games_Users_List_Fn) of
                {ok, Users_Sets} ->
                    {ok, sets:union(Users_Sets)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_opponent_statuses(Username) ->
    case get_opponent_usernames(Username) of
        {ok, Usernames} ->
            User_Status_Query = fun() ->
                Q = qlc:q([{X#user_status.username, X#user_status.is_online} ||
                              X <- mnesia:table(user_status),
                              sets:is_element(X#user_status.username, Usernames)]),
                qlc:e(Q)
            end,
            transaction(User_Status_Query);
        {error, Reason} ->
            {error, Reason}
    end.


%% Initialization

table_spec(Fields) ->
    [{type, ordered_set}, {disc_copies, [node()]}, {attributes, Fields}].

ensure_tables() ->
    mnesia:create_table(user_status, table_spec(record_info(fields, user_status))),
    mnesia:create_table(game_users, table_spec(record_info(fields, game_users))).

init(_Options) ->
    ensure_tables(),
    {ok, []}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
