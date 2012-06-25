-module(boffo_auth_passwd).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, register/2, find/1]).

-include("$BOFFO_SETTINGS").

-record(user, {username,
	       password,
	       salt,
	       timestamp}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [{file, ?BOFFO_AUTH_PASSWD_DB},
                           {type, set}],
                          []).

register(Username, Password) ->
    gen_server:call(?MODULE, {register, Username, Password}).

authenticate(Username, Password) ->
    gen_server:call(?MODULE, {authenticate, Username, Password}).

change_password(Username, Password) ->
    gen_server:call(?MODULE, {change_password, Username, Password}).

list() ->
    gen_server:call(?MODULE, {list}).

%% Call Handlers

handle_call({register, Username, Password}, _From, State) ->
    case find(Username) of
	{error, _} ->
	    {ok, Salt} = bcrypt:gen_salt(),
	    User = #user{username=Username,
			 password=bcrypt:hashpw(Password, Salt),
			 salt=Salt,
			 timestamp=now()},
	    F = fun() -> mnesia:write(User) end,
	    {reply, mnesia:transaction(F), State};
	{ok, _} ->
	    {reply, {error, "Username taken"}, State}
	end;

handle_call({authenticate, Username, Password}, _From, State) ->
    {error, "Not Implemented"};


handle_call({change_password, Username, Old_Password, New_Password}, _From, State) ->
    {error, "Not Implemented"};

handle_call({list}, _From, State) ->
    {error, "Not Implemented"}.

%% Utilities
find(Username) ->
    Q = qlc:q([X || X <- mnesia:table(user), 
		    X#user.username =:= Username]),
    case mnesia:transaction(fun() -> qlc:eval(Q) end) of
	{atomic, [User]} ->
	    {ok, User};
	{atomic, []} ->
	    {error, "Not found"};
	{aborted, Reason} ->
	    {error, Reason}
    end.

exists(Username) ->
    case find(Username) of
	{ok, _} ->
	    true;
	{error, _} ->
	    false
    end.


%% Initialization

ensure_schema() ->
    Node = node(),
    mnesia:stop(),
    Result = case mnesia:create_schema([Node]) of
		 ok ->
		     ok;
		 {error, {Node, {already_exists, Node}}} ->
		     ok;
		 {error, Error} ->
		     ok
	     end,
    mnesia:start(),
    Result.



ensure_user_table() ->
    Res = mnesia:create_table(user, [{type, ordered_set}, 
				     {disc_copies, [node()]},
				     {attributes, record_info(fields, user)}]),
    case Res of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ok
    end.

init(_Options) ->
    ensure_schema(),
    ensure_user_table(),
    {ok, []}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
