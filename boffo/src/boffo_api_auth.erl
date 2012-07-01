-module(boffo_api_auth).

-export([get_user/1, create_token/1, delete_token/1]).
-export([register/2, authenticate/2, change_password/3]).

%% Tokens

%% @doc Retrieves username associated with given token.
-spec get_user(string()) -> {ok, string()} | {error, string()}.
get_user(Token) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {get, Token}).

%% @doc Creates an auth token for given username.
-spec create_token(string()) -> {ok, string()} | {error, string()}.
create_token(Username) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {create, Username}).

%% @doc Deletes given token.
-spec delete_token(string()) -> ok | {error, string()}.
delete_token(Token) ->
    Server = pg2:get_closest_pid(boffo_auth_token_server),
    gen_server:call(Server, {delete, Token}).


%% Passwords

%% @doc Registers a user.
-spec register(string(), string()) -> {ok, ok} | {error, string()}.
register(Username, Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {register, Username, Password}).

%% @doc Authenticates a user, blocks for some amount of time on invalid credentials.
-spec authenticate(string(), string()) -> {ok, boolean()} | {error, string()}.
authenticate(Username, Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {authenticate, Username, Password}).

%% @doc Changes a user's password, blocks for some amount of time on invalid credentials.
-spec change_password(string(), string(), string()) -> {ok, ok} | {error, string()}.
change_password(Username, Old_Password, New_Password) ->
    Server = pg2:get_closest_pid(boffo_auth_passwd_server),
    gen_server:call(Server, {change_password, Username, Old_Password, New_Password}).
