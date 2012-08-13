%%-*- mode: erlang -*-
%%%-------------------------------------------------------------------
%%% File    : boffo_mgr_serv.erl
%%% Author  : Nick Fitzgerald <fitzgen@farnsworth>
%%% Description : Maps game IDs to nodes
%%%
%%% Created : 12 Aug 2012 by Nick Fitzgerald <fitzgen@farnsworth>
%%%-------------------------------------------------------------------
-module(boffo_mgr_serv).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-record(id_node, {game_id, node}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    pg2:join(boffo_mgr_server, Pid),
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
    ensure_mgr_table(),
    {ok, #state{}}.

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
handle_call({set_node, Game_Id, Node}, _From, State) ->
    {reply, save(#id_node{game_id = Game_Id, node = Node}), State};

handle_call({get_node, Game_Id}, _From, State) ->
    case find(Game_Id) of
        {ok, Id_Node} ->
            {reply, {ok, Id_Node#id_node.node}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
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

ensure_mgr_table() ->
    Res = mnesia:create_table(id_node, [{type, ordered_set},
                                        {disc_copies, [node()]},
                                        {attributes, record_info(fields, id_node)}]),
    case Res of
        {atomic, ok} ->
            ok;
        {aborted, _} ->
            ok
    end.

find(Game_Id) ->
    F = fun () -> mnesia:read(id_node, Game_Id) end,
    case mnesia:transaction(F) of
        {atomic, [Id_Node]} ->
            {ok, Id_Node};
        {atomic, []} ->
            {error, "Not found"};
        {aborted, Reason} ->
            {error, Reason}
    end.

save(Id_Node) ->
    F = fun () -> mnesia:write(Id_Node) end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.
