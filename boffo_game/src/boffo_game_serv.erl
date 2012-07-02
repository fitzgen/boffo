-module(boffo_game_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include("$BOFFO_SETTINGS").
-include_lib("boffo_game/include/boffo_game.hrl").

%% Public API

start_link() ->
    {ok, Pid} = gen_server:start_link([],
                                      []),
    pg2:join(boffo_game_server, Pid),
    {ok, Pid}.

%% Gen_server interface

handle_call({chat, {Game_Id, User, Message}}, _From, State) ->
    case find(Game_Id) of
        {ok, Game} ->
            Game1 = add_chat(Game, #chat{
                               user = User,
                               timestamp = now_to_universal_time(now()),
                               message = Message
                              }),
            F = fun () -> mnesia:write(Game1) end,
            case mnesia:transaction(F) of
                {atomic, Result} ->
                    % TODO: notify events here.
                    {reply, {ok, Result}, State};
                {aborted, Reason} ->
                    {reply, {error, Reason}, State}
            ok;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({turn, {Game_Id, User, Turn_Data}}, _From, State) ->
    case find(Game_Id) of
        {ok, Game} ->
            case add_turn(Game, User, Turn_Data) of
                {ok, Game1} ->
                    F = fun () -> mnesia:write(Game1) end,
                    case mnesia:transaction(F) of
                        {atomic, result} ->
                            % TODO: notify events here.
                            {reply, ok, State};
                        {aborted, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

handle_call({create, Game_Logic_PG, Users}, _From, State) ->
    case new_game(Game_Logic_PG, Users) of
        {ok, Game} ->
            F = fun () -> mnesia:write(Game) end,
            case mnesia:transaction(F) of
                {atomic, _Result} ->
                    % TODO: notify events here.
                    {reply, Game#game.id, State};
                {aborted, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

% handle_call({accept, {Game_Id, User}}) ->
%     todo;

% handle_call({reject, {Game_Id, User}}) ->
%     todo;

handle_call({listen, {Game_Id, Listener}}) ->
    todo.

%% Initialization

init(_Options) ->
    ensure_game_table(),
    ok.

%% Utilities

ensure_game_table() ->
    Result = mnesia:create_table(game, [{type, ordered_set},
                                        {disc_copies, [node()]},
                                        {attributes, record_info(fields, game)}]),
    case Result of
        {atomic, ok} ->
            ok;
        {aborted, _} ->
            ok
    end.

find(Game_Id) ->
    case mnesia:read(game, Game_Id) of
        {atomic, [Game]} ->
            {ok, Game};
        {atomic, []} ->
            {error, "Not found"};
        {aborted, Reason} ->
            {error, Reason}
    end.

add_chat(Game, Chat) ->
    Game#game{
      chat = [Chat|Game#game.chat],
     }.

add_turn(Game, User, Turn) ->
    case pg2:get_closest_pid(Game#game.logic_pg) of
        Server when is_pid(Server) ->
            gen_server:call(Server, {turn, User, Turn, Game});
        Error ->
            {error, Reason}
    end.

new_game(Game_Logic_PG, Users) ->
    case pg2:get_closest_pid(Game_Logic_PG) of
        Server when is_pid(Server) ->
            case gen_server:call(Server, {new_game_state, Users}) of
                {ok, Game_State} ->
                    {ok, #game{
                       id = uuid:v4(),
                       users = Users,
                       state = Game_State,
                       logic_pg = Game_Logic_PG
                      }};
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
