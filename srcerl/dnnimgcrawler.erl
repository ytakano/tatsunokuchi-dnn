%%%-------------------------------------------------------------------
%%% @author ytakano <ytakanoster@gmail.com>
%%% @copyright (C) 2010, ytakano
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2010 by ytakano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(dnnimgcrawler).

-behaviour(gen_server).

%% API
-export([start_link/2, update/0]).

-define(IMG_DIR, "/images").
-define(THUMB_DIR, "/thumbs").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {dir, home}).

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
start_link(Dir, Home) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Dir, Home], []).

update() ->
    gen_server:cast(?SERVER, update).

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
init([Dir, Home]) ->
    init(Dir, Home),
    {ok, #state{dir = Dir, home = Home}}.

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
handle_cast(update, State) ->
    update(State#state.dir, State#state.home),
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
update(Dir, Home) ->
    F = fun(File, _) ->
                gen_thumb(File, Home),
                gen_ccv(File, Dir, Home),
                gen_surf(File, Dir, Home)
        end,

    Pat = "\\.jpeg$|\\.jpg$|\\.jpe$|\\.png$|\\.bmp$|\\.dib$|\\.tiff$|\\.tif$|\\.pbm$|\\.pgm$|\\.ppm$",%",

    filelib:fold_files([Home, ?IMG_DIR], Pat, true, F, []).

gen_thumb(File, Home) ->
    FileRel = relative(filename:absname(File), filename:absname(Home)),

    Thumb = [Home, ?THUMB_DIR, FileRel, ".s.png"],

    FileTime  = filelib:last_modified(File),
    ThumbTime = filelib:last_modified(Thumb),

    if
        FileTime > ThumbTime ->
            Ref = dnnresize:resize(File, Thumb),

            receive
                {resized, Ref} ->
                    dnnfiles:add(FileRel, filelib:last_modified(File)),
                    true;
                _ ->
                    false
            end;
        true ->
            true
    end.

get_dbh_hist(File, Dir, Suffix) ->
    FileAbs = filename:absname(File),
    DirAbs  = filename:absname(Dir),

    DBH  = [DirAbs, FileAbs, Suffix, ".dbh"],
    Hist = [DirAbs, FileAbs, Suffix],

    {FileAbs, DBH, Hist}.

gen_ccv(File, Dir, Home) ->
    {FileAbs, DBH, Hist} = get_dbh_hist(File, Dir, ".ccv.hist"),

    FileTime = filelib:last_modified(File),
    DBHTime  = filelib:last_modified(DBH),

    if
        FileTime > DBHTime ->
            Ref = dnnhist:create(ccv_hist, File),
            receive
                {created_hist, Ref} ->
                    dnndbh:create(ccv_dbh, Hist)
            end,

            FileRel = relative(FileAbs, filename:absname(Home)),
            dnnsim:add(ccv_sim, FileRel, DBH, Hist);
        true ->
            ok
    end.

gen_surf(File, Dir, Home) ->
    {FileAbs, DBH, Hist} = get_dbh_hist(File, Dir, ".surf.hist"),

    FileTime = filelib:last_modified(File),
    DBHTime  = filelib:last_modified(DBH),

    if
        FileTime > DBHTime ->
            Ref = dnnhist:create(surf_hist, File),
            receive
                {created_hist, Ref} ->
                    dnndbh:create(surf_dbh, Hist)
            end,

            FileRel = relative(FileAbs, filename:absname(Home)),
            dnnsim:add(surf_sim, FileRel, DBH, Hist);
        true ->
            ok
    end.

relative([H1 | T1], [H2 | T2])
  when H1 == H2 ->
    relative(T1, T2);
relative(File, _) ->
    File.

init(Dir, Home) ->
    F = fun(File, _) ->
                FileAbs = filename:absname(File),
                DirAbs  = filename:absname(Dir),

                CCVDBH  = [DirAbs, FileAbs, ".ccv.hist.dbh"],
                SURFDBH = [DirAbs, FileAbs, ".surf.hist.dbh"],

                CCVHIST  = [DirAbs, FileAbs, ".ccv.hist"],
                SURFHIST = [DirAbs, FileAbs, ".surf.hist"],

                FileRel = relative(FileAbs, filename:absname(Home)),

                gen_thumb(File, Home),

                dnnfiles:add(FileRel, filelib:last_modified(File)),

                dnnsim:add(ccv_sim, FileRel, CCVDBH, CCVHIST),
                dnnsim:add(surf_sim, FileRel, SURFDBH, SURFHIST)
        end,

    Pat = "\\.jpeg$|\\.jpg$|\\.jpe$|\\.png$|\\.bmp$|\\.dib$|\\.tiff$|\\.tif$|\\.pbm$|\\.pgm$|\\.ppm$",%",

    filelib:fold_files([Home, ?IMG_DIR], Pat, true, F, []).

