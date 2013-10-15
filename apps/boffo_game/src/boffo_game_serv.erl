%%-*- mode: erlang -*-
-module(boffo_game_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include("$BOFFO_SETTINGS").
-include("../../boffo/include/boffo.hrl").

%% Public API

start_link() ->
    gen_server:start_link(?MODULE,
                          [],
                          []).

%% Gen_server interface

handle_call({chat, {Game_Id, User, Message}}, _From, State) ->
    case find_with_user(Game_Id, User) of
        {ok, Game} ->
            Game1 = add_chat(Game, #chat{
                               user = User,
                               timestamp = calendar:now_to_universal_time(now()),
                               message = Message
                              }),
            F = fun () -> mnesia:write(Game1) end,
            case boffo_util:transaction(F) of
                {ok, _Result} ->
                    % TODO: notify events here.
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({turn, {Game_Id, User, Turn_Data}}, _From, State) ->
    case find_with_user(Game_Id, User) of
        {ok, Game} ->
            case add_turn(Game, User, Turn_Data) of
                {ok, Game1} ->
                    Game2 = Game1#game{
                              turns = [{User, Turn_Data} | Game1#game.turns]
                             },
                    F = fun () -> mnesia:write(Game2) end,
                    case boffo_util:transaction(F) of
                        {ok, _Result} ->
                            % TODO: notify events here.
                            {reply, ok, State};
                        {error, Reason} ->
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
            case boffo_util:transaction(F) of
                {ok, _Result} ->
                    % TODO: notify events here.
                    {reply, {ok, Game#game.id}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get, Game_Id}, _From, State) ->
    case find(Game_Id) of
        {ok, Game} ->
            {reply, {ok, Game}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;


%% TODO: Accepting or rejecting games

% handle_call({accept, {Game_Id, User}}) ->
%     todo;

% handle_call({reject, {Game_Id, User}}) ->
%     todo;

handle_call({listen, {Game_Id, Listener}}, _From, State) ->
    Game_Id,
    Listener,
    State,
    todo.

%% Initialization

init(_Options) ->
    pg2:create(?MODULE),
    pg2:join(?MODULE, self()),
    pg2:create({node(), ?MODULE}),
    pg2:join({node(), ?MODULE}, self()),

    ensure_game_table(),

    {ok, []}.

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
    F = fun () -> mnesia:read(game, Game_Id) end,
    boffo_util:single_result(boffo_util:transaction(F)).

user_in_game(User, Game) ->
    sets:is_element(User, Game#game.players).

find_with_user(Game_Id, User) ->
    case find(Game_Id) of
        {ok, Game} ->
            case user_in_game(User, Game) of
                true ->
                    {ok, Game};
                false ->
                    {error, user_not_in_game}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

add_chat(Game, Chat) ->
    Game#game{
      chat = [Chat|Game#game.chat]
     }.

add_turn(Game, User, Turn) ->
    case pg2:get_closest_pid(Game#game.logic_pg) of
        Server when is_pid(Server) ->
            gen_server:call(Server, {turn, User, Turn, Game});
        % Avoid nested {error, {error, ...}} responses.
        {error, Error} ->
            {error, Error};
        Error ->
            {error, Error}
    end.

new_game(Game_Logic_PG, Users) ->
    case pg2:get_closest_pid(Game_Logic_PG) of
        Server when is_pid(Server) ->
            case gen_server:call(Server, {new_game_state, Users}) of
                {ok, Game_State} ->
                    {ok, #game{
                       id = uuid:v4(),
                       players = Users,
                       state = Game_State,
                       logic_pg = Game_Logic_PG
                      }};
                {error, Error} ->
                    {error, Error}
            end;
        % Avoid nested {error, {error, ...}} responses.
        {error, Error} ->
            {error, Error};
        Error ->
            {error, Error}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
