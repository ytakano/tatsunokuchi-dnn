%%%-------------------------------------------------------------------
%%% @author ytakano <ytakanoster@gmail.com>
%%% @copyright (C) 2010, ytakano
%%% @doc
%%%
%%% @end
%%% Created : 11 Jul 2010 by ytakano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(dnnrndimgs).

-behaviour(gen_server).

%% API
-export([start_link/0, get_images/0]).

-define(RND_ARR, [1, 2, 3, 5, 7, 11, 13, 17, 19, 23]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_images() ->
    gen_server:call(?SERVER, get).

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
init([]) ->
    {ok, #state{}}.

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
handle_call(get, _From, State) ->
    skip_imgs(?RND_ARR),
    Reply = get_imgs(?RND_ARR, []),
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
terminate(_Reason, _State) ->
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
get_imgs([], Files) ->
    Files;
get_imgs([H | T], Files) ->
    case get(H) of
        undefined ->
            get_imgs(T, Files);
        '$end_of_table' ->
            get_imgs(T, Files);
        F ->
            get_imgs(T, [F | Files])
    end.

%%--------------------------------------------------------------------
skip_imgs([]) ->
    ok;
skip_imgs([H | T]) ->
    File = case get(H) of
               '$end_of_table' ->
                   skip(ets:first(files), H - 1);
               undefined ->
                   skip(ets:first(files), H - 1);
               F ->
                   skip(F, H)
           end,

    put(H, File),

    skip_imgs(T).

%%--------------------------------------------------------------------
skip('$end_of_table', N) ->
    case ets:first(files) of
        '$end_of_table' ->
            '$end_of_table';
        F ->
            skip(F, N)
    end;
skip(File, N)
  when N > 0 ->
    F = ets:next(files, File),
    skip(F, N - 1);
skip(File, _) ->
    File.
