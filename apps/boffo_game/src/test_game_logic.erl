%%%-------------------------------------------------------------------
%%% File    : test_game_logic.erl
%%% Author  : Nick Fitzgerald <fitzgen@farnsworth>
%%% Description : A game logic module written for testing.
%%%
%%% Created :  7 Jul 2012 by Nick Fitzgerald <fitzgen@farnsworth>
%%%-------------------------------------------------------------------
-module(test_game_logic).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("../../boffo/include/boffo.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    pg2:join(test_game_logic, Pid),
    {ok, Pid}.

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({new_game_state, _Users}, _From, State) ->
    {reply, {ok, 0}, State};

handle_call({turn, User, Turn, Game}, _From, State) ->
    case Turn of
        _ when is_integer(Turn) ->
            New_State = Game#game.state + Turn,
            case New_State of
                _ when New_State > 10 ->
                    {reply,
                     {ok, Game#game{
                            state = New_State,
                            victor = User
                           }},
                     State};
                _ ->
                    {reply,
                     {ok, Game#game{
                            state = New_State
                           }},
                     State}
            end;
        _ ->
            {reply, {error, "Expected an integer"}, State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
