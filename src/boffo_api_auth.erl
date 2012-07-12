-module(boffo_api_auth).

-export([get_user/1, create_token/1, delete_token/1]).
-export([register/2, authenticate/2, change_password/3]).

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

register(Username, Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {register, Username, Password}).

authenticate(Username, Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {authenticate, Username, Password}).

change_password(Username, Old_Password, New_Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {change_password, Username, Old_Password, New_Password}).
