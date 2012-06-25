-module(boffo_api_auth).

-export([get_user/1, create_token/1, delete_token/1]).

%% Tokens

get_user(Token) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {get, Token}).

create_token(User) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {create, User}).

delete_token(Token) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {delete, Token}).


%% Passwords
