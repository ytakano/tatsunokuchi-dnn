-module(dnnweb).
-export([start/1, get_hash_path/0]).

start([Conf]) ->
    dnnenv:start_link(),

    Json = open_json(Conf),

    run_ccv(Json),
    run_surf(Json),
    run_resize(Json),
    run_yaws(Json),
    dnnfiles:start_link(),
    run_imgcrawler(Json),
    dnnrndimgs:start_link().

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
open_json(Conf) ->
    try file:read_file(Conf) of
        {ok, Str} ->
            StrList = binary_to_list(Str),
            try json:decode_string(StrList) of
                {ok, Json} ->
                    Json;
                _ ->
                    io:format("invalid config file\n"),
                    exit({dnnweb, stopped})
            catch
                _:W ->
                    io:format("internal error : catched '~p' when parsing\n~s\n",
                              [W, StrList]),
                    exit({dnnweb, stopped})
            end;
        _ ->
            io:format("failed to open \"~s\"", [Conf]),
            exit({dnnweb, stopped})
    catch
        _:W ->
            io:format("internal error: catched '~p' when reading '~s'\n",
                      [W, Conf]),
            exit({dnnweb, stopped})
    end.

%%--------------------------------------------------------------------
run_yaws(Json) ->
    Home = case jsonrpc:s(Json, home) of
               H when is_list(H) ->
                   H;
               _ ->
                   "public_html"
           end,

    Port = case jsonrpc:s(Json, port) of
               P when is_integer(P) ->
                   P;
               _ ->
                   7100
           end,

    case jsonrpc:s(Json, logdir) of
        Dir when is_list(Dir) ->
            yaws:start_embedded(Home, [{port, Port}], [{logdir, Dir}]);
        _ ->
            yaws:start_embedded(Home, [{port, Port}])
    end.

%%--------------------------------------------------------------------
run_imgcrawler(Json) ->
    Home = case jsonrpc:s(Json, home) of
               H when is_list(H) ->
                   H;
               _ ->
                   "public_html"
           end,

    Feat = case jsonrpc:s(Json, featdir) of
               F when is_list(F) ->
                   F;
               _ ->
                   "features"
           end,

    dnnenv:insert({home, filename:absname(Home)}),
    dnnenv:insert({dir, filename:absname(Feat)}),

    dnnimgcrawler:start_link(Feat, Home),
    dnnimgcrawler:update().

%%--------------------------------------------------------------------
print_run_error(Cmd) ->
    io:format("cannot run ~s\n", [Cmd]).


%%--------------------------------------------------------------------
%%
%% run process to find similar objects by using the color coherence vector
%%
%%--------------------------------------------------------------------
run_ccv(Json) ->
    case jsonrpc:s(Json, ccv) of
        undefiled ->
            print_run_error("dnn_ccv");
        J ->
            run_ccv_hist(J),
            run_ccv_dbh(J),
            run_ccv_sim(J)
    end.

run_ccv_hist(Json) ->
    case jsonrpc:s(Json, hist) of
        Hist when is_list(Hist) ->
            dnnhist:start_link(ccv_hist, Hist);
        _ ->
            print_run_error("ccv_hist")
    end.

run_ccv_dbh(Json) ->
    case jsonrpc:s(Json, dbh) of
        DBH when is_list(DBH) ->
            dnndbh:start_link(ccv_dbh, DBH);
        _ ->
            print_run_error("ccv_dbh")
    end.

run_ccv_sim(Json) ->
    case jsonrpc:s(Json, sim) of
        SIM when is_list(SIM) ->
            dnnsim:start_link(ccv_sim, SIM),
            dnnsim:set_threshold(ccv_sim, 0.15);
        _ ->
            print_run_error("ccv_sim")
    end.


%%--------------------------------------------------------------------
%%
%% run process to find similar objects by using the SURF
%%
%%--------------------------------------------------------------------
run_surf(Json) ->
    case jsonrpc:s(Json, surf) of
        undefiled ->
            print_run_error("dnn_surf");
        J ->
            run_surf_hist(J),
            run_surf_dbh(J),
            run_surf_sim(J)
    end.

run_surf_hist(Json) ->
    case jsonrpc:s(Json, hist) of
        Hist when is_list(Hist) ->
            dnnhist:start_link(surf_hist, Hist);
        _ ->
            print_run_error("surf_hist")
    end.

run_surf_dbh(Json) ->
    case jsonrpc:s(Json, dbh) of
        DBH when is_list(DBH) ->
            dnndbh:start_link(surf_dbh, DBH);
        _ ->
            print_run_error("surf_dbh")
    end.

run_surf_sim(Json) ->
    case jsonrpc:s(Json, sim) of
        SIM when is_list(SIM) ->
            dnnsim:start_link(surf_sim, SIM),
            dnnsim:set_threshold(surf_sim, 0.005);
        _ ->
            print_run_error("surf_sim")
    end.

%%--------------------------------------------------------------------
%%
%% run process to resize images
%%
%%--------------------------------------------------------------------
run_resize(Json) ->
    case jsonrpc:s(Json, resize) of
        Cmd when is_list(Cmd) ->
            dnnresize:start_link(Cmd);
        _ ->
            print_run_error("dnn_resize")
    end.
