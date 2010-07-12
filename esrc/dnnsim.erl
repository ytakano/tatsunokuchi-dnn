%%%-------------------------------------------------------------------
%%% @author ytakano <ytakanoster@gmail.com>
%%% @copyright (C) 2010, ytakano
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2010 by ytakano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(dnnsim).

-behaviour(gen_server).

%% API
-export([start_link/2, add/4, find/3, set_threshold/2, clear_cache/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cmd, port, cache}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Server, Cmd) ->
    gen_server:start_link({local, Server}, ?MODULE, [Cmd], []).

add(Server, Str, Hash, Hist) ->
    gen_server:cast(Server, {add, Str, Hash, Hist}).

find(Server, Hash, Hist) ->
    gen_server:call(Server, {get, Hash, Hist}).

set_threshold(Server, Threshold) ->
    gen_server:cast(Server, {threshold, Threshold}).

clear_cache(Server) ->
    gen_server:call(Server, clear_cache).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Cmd]) ->
    TID = ets:new(?MODULE, []),

    try open_port({spawn, Cmd}, [{line, 1024}]) of
        Port ->
            {ok, #state{cmd = Cmd, port = Port, cache = TID}}
    catch
        _:Why ->
            {stop, Why}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Hash, Hist}, _From, State) ->
    Reply = find_similar(State#state.port, State#state.cache, Hash, Hist),
    {reply, Reply, State};
handle_call(clear_cache, _From, State) ->
    ets:delete_all_objects(State#state.cache),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Str, Hash, Hist}, State) ->
    insert(State#state.port, Str, Hash, Hist),
    {noreply, State};
handle_cast({threshold, Threshold}, State) ->
    set_threshold_in(State#state.port, Threshold),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert(Port, Str, Hash, Hist) ->
    Port ! {self(), {command, "add\n"}},

    receive
        {Port, {data, {eol, _}}} ->
            insert_str(Port, Str, Hash, Hist)
    end.

insert_str(Port, Str, Hash, Hist) ->
    Port ! {self(), {command, [Str, "\n"]}},

    receive
        {Port, {data, {eol, _}}} ->
            insert_hash(Port, Hash, Hist)
    end.

insert_hash(Port, Hash, Hist) ->
    Port ! {self(), {command, [Hash, "\n"]}},

    receive
        {Port, {data, {eol, _}}} ->
            insert_hist(Port, Hist)
    end.

insert_hist(Port, Hist) ->
    Port ! {self(), {command, [Hist, "\n"]}},

    receive
        {Port, {data, {eol, "true"}}} ->
            true;
        {Port, {data, {eol, "false"}}} ->
            false
    end.

%%--------------------------------------------------------------------
find_similar(Port, TID, Hash, Hist) ->
    case ets:lookup(TID, Hash) of
        [{Hash, Data}] ->
            Data;
        [] ->
            Data = find_similar(Port, Hash, Hist),
            ets:insert(TID, {Hash, Data}),
            Data
    end.

find_similar(Port, Hash, Hist) ->
    Port ! {self(), {command, "get\n"}},

    receive
        {Port, {data, {eol, _}}} ->
            find_similar_hash(Port, Hash, Hist)
    end.

find_similar_hash(Port, Hash, Hist) ->
    Port ! {self(), {command, [Hash, "\n"]}},

    receive
        {Port, {data, {eol, _}}} ->
            find_similar_hist(Port, Hist)
    end.

find_similar_hist(Port, Hist) ->
    Port ! {self(), {command, [Hist, "\n"]}},

    recv_similar(Port, []).

recv_similar(Port, List) ->
    receive
        {Port, {data, {eol, "."}}} ->
            lists:reverse(List);
        {Port, {data, {eol, Data}}} ->
            Port ! {self(), {command, "next\n"}},
            recv_similar(Port, [Data | List])
    end.

%%--------------------------------------------------------------------
set_threshold_in(Port, Threshold) ->
    Port ! {self(), {command, "threshold\n"}},

    receive
        {Port, {data, {eol, _}}} ->
            F = io_lib:format("~p\n", [Threshold]),
            Port ! {self(), {command, F}},

            receive
                {Port, {data, {eol, "true"}}} ->
                    true;
                {Port, {data, {eol, "false"}}} ->
                    false
            end
    end.
