-module(boffo_auth_passwd).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, register/2, find/1, authenticate/2, change_password/3]).

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

change_password(Username, Old_Password, New_Password) ->
    gen_server:call(?MODULE, {change_password, Username, Old_Password, New_Password}).

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
	    case mnesia:transaction(F) of
		{atomic, Result} ->
		    {reply, {ok, Result}, State};
		{aborted, Reason} ->
		    {reply, {error, Reason}, State}
	    end;
	{ok, _} ->
	    {reply, {error, "Username taken"}, State}
	end;

handle_call({authenticate, Username, Password}, _From, State) ->
    case authenticate_user(Username, Password) of
	{ok, true} ->
	    {reply, {ok, true}, State};
	{ok, false} ->
	    {reply, {ok, false}, State}
    end;

handle_call({change_password, Username, Old_Password, New_Password}, _From, State) ->
    case authenticate_user(Username, Old_Password) of
	{ok, true} ->
	    {ok, User} = find(Username),
	    {ok, New_Salt} = bcrypt:gen_salt(),
	    F = fun() -> 
	        mnesia:write(User#user{password=bcrypt:hashpw(New_Password, New_Salt), 
				       salt=New_Salt}) 
	    end,
	    case mnesia:transaction(F) of
		{atomic, Result} ->
		    {reply, {ok, Result}, State};
		{aborted, Reason} ->
		    {reply, {error, Reason}, State}
	    end;
	{ok, false} ->
	    {reply, {error, "Old password does not match"}, State};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;


handle_call({list}, _From, _State) ->
    {error, "Not Implemented"}.

%% Utilities
authenticate_user(Username, Password) ->
    case find(Username) of
	{ok, User} ->
	    Pid = self(),
	    Check_Auth = fun() ->
                Actual = User#user.password,				 
                case bcrypt:hashpw(Password, User#user.salt) of
		    Actual ->
			Pid ! User;
		    _ ->
			ok
		end
	    end,
	    spawn(Check_Auth),
	    receive
		User ->
		    {ok, true}
	    after 2000 ->
		    {ok, false}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

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
		     {error, Error}
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
	{aborted, _} ->
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
