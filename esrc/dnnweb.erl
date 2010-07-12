%%%-------------------------------------------------------------------
%%% @author ytakano <ytakano@mementomori.local>
%%% @copyright (C) 2010, ytakano
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2010 by ytakano <ytakano@mementomori.local>
%%%-------------------------------------------------------------------
-module(dnnweb).

-behaviour(supervisor).

%% API
-export([start_link/1, get_hash_path/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-record(conf, {home,
               port = 7100,
               logdir, featdir,
               resize,
               ccv_hist, ccv_dbh, ccv_sim,
               surf_hist, surf_dbh, surf_sim}).


%%%===================================================================
%%% API functions
%%%===================================================================
get_hash_path() ->
    case ets:lookup(env, home) of
        [{home, Home}] ->
            case ets:lookup(env, dir) of
                [{dir, Dir}] ->
                    {Dir, Home};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([Conf]) ->
    open_json(Conf).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Conf]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [
                {tag1, {dnnenv, start_link, []},
                 Restart, Shutdown, Type, [dnnenv]},

                {tag2, {dnnhist, start_link, [ccv_hist, Conf#conf.ccv_hist]},
                 Restart, Shutdown, Type, [dnnhist]},
                {tag3, {dnndbh, start_link, [ccv_dbh, Conf#conf.ccv_dbh]},
                 Restart, Shutdown, Type, [dnndbh]},
                {tag4, {dnnsim, start_link, [ccv_sim, Conf#conf.ccv_sim]},
                 Restart, Shutdown, Type, [dnnsim]},

                {tag5, {dnnhist, start_link, [surf_hist, Conf#conf.surf_hist]},
                 Restart, Shutdown, Type, [dnnhist]},
                {tag6, {dnndbh, start_link, [surf_dbh, Conf#conf.surf_dbh]},
                 Restart, Shutdown, Type, [dnndbh]},
                {tag7, {dnnsim, start_link, [surf_sim, Conf#conf.surf_sim]},
                 Restart, Shutdown, Type, [dnnsim]},

                {tag8, {dnnfiles, start_link, []},
                 Restart, Shutdown, Type, [dnnfiles]},

                {tag9, {dnnimgcrawler, start_link,
                        [Conf#conf.featdir, Conf#conf.home]},
                 Restart, Shutdown, Type, [dnnfiles]},

                {tag10, {dnnrndimgs, start_link, []},
                 Restart, Shutdown, Type, [dnnrndimgs]}
               ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_json(Conf) ->
    try file:read_file(Conf) of
        {ok, Str} ->
            StrList = binary_to_list(Str),
            try json:decode_string(StrList) of
                {ok, Json} ->
                    read_ccv(Json);
                _ ->
                    {error, "invalid config file\n"}
            catch
                _:W ->
                    {error, W}
            end;
        _ ->
            {error, "failed to open"}
    catch
        _:W ->
            {error, W}
    end.

%%--------------------------------------------------------------------
read_ccv(Json) ->
    case jsonrpc:s(Json, ccv) of
        undefiled ->
            {error, "ccv field is undefined"};
        CCV ->
            read_ccv_hist(Json, CCV)
    end.

read_ccv_hist(Json, CCV) ->
    case jsonrpc:s(CCV, hist) of
        undefiled ->
            {error, "hist field is required in ccv"};
        CCV_Hist ->
            read_ccv_dbh(Json, CCV, #conf{ccv_hist = CCV_Hist})
    end.

read_ccv_dbh(Json, CCV, Conf) ->
    case jsonrpc:s(CCV, dbh) of
        undefiled ->
            {error, "dbh field is required in ccv"};
        CCV_DBH ->
            read_ccv_sim(Json, CCV, Conf#conf{ccv_dbh = CCV_DBH})
    end.

read_ccv_sim(Json, CCV, Conf) ->
    case jsonrpc:s(CCV, sim) of
        undefiled ->
            {error, "sim field is required in ccv"};
        CCV_Sim ->
            read_surf(Json, Conf#conf{ccv_sim = CCV_Sim})
    end.

%%--------------------------------------------------------------------
read_surf(Json, Conf) ->
    case jsonrpc:s(Json, surf) of
        undefiled ->
            {error, "surf field is undefined"};
        Surf ->
            read_surf_hist(Json, Surf, Conf)
    end.

read_surf_hist(Json, Surf, Conf) ->
    case jsonrpc:s(Surf, hist) of
        undefiled ->
            {error, "hist field is required in surf"};
        SURF_Hist ->
            read_surf_dbh(Json, Surf, Conf#conf{surf_hist = SURF_Hist})
    end.

read_surf_dbh(Json, Surf, Conf) ->
    case jsonrpc:s(Surf, dbh) of
        undefiled ->
            {error, "dbh field is required in surf"};
        SURF_DBH ->
            read_surf_sim(Json, Surf, Conf#conf{surf_dbh = SURF_DBH})
    end.

read_surf_sim(Json, Surf, Conf) ->
    case jsonrpc:s(Surf, sim) of
        undefiled ->
            {error, "sim field is required in surf"};
        SURF_Sim ->
            read_home(Json, Conf#conf{surf_sim = SURF_Sim})
    end.

%%--------------------------------------------------------------------
read_home(Json, Conf) ->
    case jsonrpc:s(Json, home) of
        undefiled ->
            {error, "home field is undefined"};
        Home ->
            read_featdir(Json, Conf#conf{home = Home})
    end.

%%--------------------------------------------------------------------
read_featdir(Json, Conf) ->
    case jsonrpc:s(Json, featdir) of
        undefiled ->
            {error, "featdir field is undefined"};
        Dir ->
            read_logdir(Json, Conf#conf{featdir = Dir})
    end.

%%--------------------------------------------------------------------
read_logdir(Json, Conf) ->
    case jsonrpc:s(Json, logdir) of
        undefiled ->
            {error, "logdir field is undefined"};
        Dir ->
            read_port(Json, Conf#conf{logdir = Dir})
    end.

%%--------------------------------------------------------------------
read_port(Json, Conf) ->
    case jsonrpc:s(Json, port) of
        undefiled ->
            read_resize(Json, Conf);
        Port ->
            read_resize(Json, Conf#conf{port = Port})
    end.

%%--------------------------------------------------------------------
read_resize(Json, Conf) ->
    case jsonrpc:s(Json, resize) of
        undefiled ->
            {error, "resize field is undefined"};
        Resize ->
            start_link_with_conf(Conf#conf{resize = Resize})
    end.

%%--------------------------------------------------------------------
start_link_with_conf(Conf) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Conf]) of
        {ok, _} = Ret ->
            dnnimgcrawler:initialize(),

            yaws:start_embedded(Conf#conf.home, [{port, Conf#conf.port},
                                                 {listen, {0, 0, 0, 0}}]),

            dnnenv:insert({home, filename:absname(Conf#conf.home)}),
            dnnenv:insert({dir, filename:absname(Conf#conf.featdir)}),

            Ret;
        Ret ->
            Ret
    end.
