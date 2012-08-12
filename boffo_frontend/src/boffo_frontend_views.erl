-module(boffo_frontend_views).
-export([urls/0]).

-compile(export_all).

urls() -> [
    {["^api/login/?" | [$$]], fun login/2},
    {["^api/logout/?" | [$$]], fun logout/2},
    {["^api/feed/?" | [$$]], fun start_feed/2},
    {["^api/msg/?" | [$$]], fun get_msg/2}
].

% render_ok(Req, TemplateModule, Params) ->
%     {ok, Output} = TemplateModule:render(Params),
%     % Here we use mochiweb_request:ok/1 to render a reponse
%     Req:ok({"text/html", Output}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% login view
login('POST', Req) ->
    Params = ["username", "password"],
    case retrieve_all(Params, Req:parse_post()) of
        [{"username", Username}, {"password", Password}] ->
            handle_login(Req, Username, Password);
        error ->
            invalid_params_response(Req, Params)
    end.

handle_login(Req, Username, Password) ->
    case get_auth_token(Username, Password) of
        {ok, Token} ->
            json_response(Req, auth_json(Token));
        {error, Error} ->
            error_response(Req, Error)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% logout view
logout('POST', Req) ->
    Params = ["token"],
    case retrieve_all(Params, Req:parse_post()) of
        [{"token", Token}] ->
            handle_logout(Req, Token);
        error ->
            invalid_params_response(Req, Params)
    end.

handle_logout(Req, Token) ->
    case check_token(Token) of
        {ok, _} ->
            % close the feed
            io:format("close token: ~s~n", [Token]),
            ok_response(Req);
        {error, Error} ->
            error_response(Req, Error)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_feed view
start_feed('POST', Req) ->
    Params = ["token"],
    case retrieve_all(Params, Req:parse_post()) of
        [{"token", Token}] ->
            handle_start_feed(Req, Token);
        error ->
            invalid_params_response(Req, Params)
    end.

% ACTUAL CODE: UNCOMMENTED IS FOR TESTING
% handle_start_feed(Req, Token) ->
%     case check_token(Token) of
%         {ok, Username} ->
%             Response = feed_response(Req),
%             Feed = create_feed(Username),
%             join_feed(Feed, Response);
%         {error, Reason} ->
%             error_response(Req, Reason)
%     end.

handle_start_feed(Req, Token) ->
    case check_token(Token) of
        {ok, _} ->
            Response = feed_response(Req),
            %Feed = create_feed("chat"),
            Feed = "chat",
            join_feed(Feed, Response);
        {error, Reason} ->
            error_response(Req, Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_msg view
get_msg('POST', Req) ->
    Params = ["token", "msg"],
    case retrieve_all(Params, Req:parse_post()) of
        [{"token", Token}, {"msg", Msg}] ->
            handle_get_msg(Req, Token, Msg);
        error ->
            invalid_params_response(Req, Params)
    end.

% NOTE: next two functions are for testing only
handle_get_msg(Req, Token, Msg) ->
    case check_token(Token) of
        {ok, Username} ->
            push_event("chat", <<"msg">>, msg_struct(Username, Msg)),
            ok_response(Req);
        {error, Reason} ->
            error_response(Req, Reason)
    end.

msg_struct(Username, Msg) ->
    {struct, [{<<"msg">>, StrMsg}]} = json_decode(Msg),
    {struct, [{<<"username">>, list_to_binary(Username)},
              {<<"msg">>, StrMsg}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% responses
feed_response(Req) ->
    Req:ok({"text/json; charset=utf-8",
            [{"Server","BoffoServ"}],
            chunked}).

json_response(Req, JsonMsg) ->
    Req:respond({200, [{"Content-Type", "text/json"}], JsonMsg}).

error_response(Req, Msg) ->
    json_response(Req, error_json(Msg)).

ok_response(Req) ->
    json_response(Req, ok_json()).

invalid_params_response(Req, Params) ->
    ParamsStr = string:join(Params, ", "),
    error_response(Req, io_lib:format("POST params required: ~s", [ParamsStr])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% json utils
feed_json(Type, JsonTerm) ->
    JsonData = {struct, [{<<"type">>, Type},
                         {<<"data">>, JsonTerm}]},
    json_encode(JsonData).


% Msg::string()
error_json(Msg) ->
    JsonData = {struct, [{<<"error">>, list_to_binary(Msg)}]},
    json_encode(JsonData).

ok_json() ->
    JsonData = {struct, [{<<"status">>, <<"ok">>}]},
    json_encode(JsonData).

auth_json(Token) ->
    JsonData = {struct, [{<<"token">>, Token}]},
    json_encode(JsonData).

json_encode(Term) ->
    mochijson2:encode(Term).

json_decode(Term) ->
    mochijson2:decode(Term).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% auth utils
authenticate(Username, Password) ->
    boffo_api_auth:authenticate(Username, Password).

get_auth_token(Username, Password) ->
    case authenticate(Username, Password) of
        {ok, true} ->
            Token = mk_token(Username),
            {ok, list_to_binary(Token)};
        {ok, false} ->
            {error, "Invalid credentials"};
        {error, Error} ->
            {error, Error}
    end.

mk_token(Username) ->
    {ok, Token} = boffo_api_auth:create_token(Username),
    Token.

check_token(Token) ->
    boffo_api_auth:get_user(Token).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% feed utils
create_feed(Username) ->
    boffo_api_feeds:create_feed(Username, boffo_frontend_feed),
    Username.

join_feed(Username, Resp) ->
    boffo_api_feeds:join_feed(Username, self(), Resp).

push_event(Username, Type, JsonTerm) ->
    boffo_api_feeds:push_event(Username, feed_json(Type, JsonTerm)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utils
retrieve_all(Keys, PList) ->
    Prop_Or_None = fun(Key) ->
                           proplists:lookup(Key, PList)
                   end,
    Result = lists:map(Prop_Or_None, Keys),
    case lists:member(none, Result) of
        true ->
            error;
        _ ->
            Result
    end.
