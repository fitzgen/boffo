-module(boffo_auth_app).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, []).

start(_StartType, _StartArgs) ->
    crypto:start(),
    bcrypt:start(),

    boffo_util:ensure_mnesia_schema(node()),
    mnesia:start(),

    pg2:create(boffo_auth_token_server),
    pg2:create(boffo_auth_passwd_server),

    boffo_auth_sup:start_link().



stop(_State) ->
    ok.
