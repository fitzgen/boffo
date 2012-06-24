-module(boffo_auth_token).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get_user/1, create_token/1, delete_token/1]).

-include("$BOFFO_SETTINGS").

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [{file, ?BOFFO_AUTH_TOKEN_DB},
                           {type, set}],
                          []).

get_user(Token) ->
    gen_server:call(?MODULE, {get, Token}).

create_token(User) ->
    gen_server:call(?MODULE, {create, User}).

delete_token(Token) ->
    gen_server:call(?MODULE, {delete, Token}).


%% Gen_server interface

init(Options) ->
    dets:open_file(?MODULE, Options).

handle_call({get, Token}, _From, Table) ->
    case dets:lookup(Table, Token) of
        [] ->
            {reply, {error, "Does not exist"}, Table};
        [{Token, User}] ->
            {reply, {ok, User}, Table};
        {error, Error} ->
            {reply, {error, Error}, Table}
    end;
handle_call({create, User}, _From, Table) ->
    Token = uuid:to_string(uuid:v4()),
    case dets:insert(Table, {Token, User}) of
        ok ->
            {reply, {ok, Token}, Table};
        {error, Error} ->
            {reply, {error, Error}, Table}
    end;
handle_call({delete, Token}, _From, Table) ->
    {reply, dets:delete(Table, Token), Table}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
