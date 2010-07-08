-module(dnnweb).
-export([start/1]).

start([Conf]) ->
    process_flag(trap_exit, true),
    dnnfiles:start(),
    open_json(Conf),
    loop().

loop() ->
    receive
        {'EXIT', _, Reason} ->
            yaws:stop(),

            io:format("stopped: ~p\n", [Reason]),

            catch runcmd:stop(ccv),
            catch runcmd:stop(ccv),
            catch runcmd:stop(ccv_dbh),
            catch runcmd:stop(ccv_sim),
            catch runcmd:stop(surf),
            catch runcmd:stop(surf_dbh),
            catch runcmd:stop(surf_sim),
            catch dnnfiles:stop(),
            catch dnnimgs:stop(),

            exit({dnnweb, stopped})
    end.

open_json(Conf) ->
    try file:read_file(Conf) of
        {ok, Str} ->
            read_json(Str);
        _ ->
            io:format("failed to open \"~s\"", [Conf]),
            exit({dnnweb, stopped})
    catch
        _:W ->
            io:format("internal error: catched '~p' when reading '~s'\n",
                      [W, Conf]),
            exit({dnnweb, stopped})
    end.

read_json(Str) ->
    StrList = binary_to_list(Str),
    try json:decode_string(StrList) of
        {ok, Json} ->
            run_web(Json),
            run_ccv(Json),
            run_surf(Json),
            run_imgs(Json);
        _ ->
            io:format("invalid config file\n"),
            exit({dnnweb, stopped})
    catch
        _:W ->
            io:format("internal error : catched '~p' when parsing\n~s\n",
                      [W, StrList]),
            exit({dnnweb, stopped})
    end.

run_web(Json) ->
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


run_imgs(Json) ->
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

    dnnimgs:start(Feat, Home, 6).


print_run_error(Cmd) ->
    io:format("cannot run ~s\n", [Cmd]).


% run process to find similar objects by using the color coherence vector
run_ccv(Json) ->
    case jsonrpc:s(Json, ccv) of
        undefiled ->
            print_run_error("dnn_ccv");
        J ->
            run_ccv_cmd(J)
    end.

run_ccv_cmd(Json) ->
    case jsonrpc:s(Json, cmd) of
        Cmd when is_list(Cmd) ->
            run_ccv_dbh(Json, Cmd);
        _ ->
            print_run_error("dnn_ccv")
    end.

run_ccv_dbh(Json, Cmd) ->
    case jsonrpc:s(Json, dbh) of
        DBH when is_list(DBH) ->
            run_ccv_sim(Json, Cmd, DBH);
        _ ->
            print_run_error("dnn_ccv")
    end.

run_ccv_sim(Json, Cmd, DBH) ->
    case jsonrpc:s(Json, sim) of
        SIM when is_list(SIM) ->
            runcmd:start(ccv, Cmd),
            runcmd:start(ccv_dbh, DBH),
            runcmd:start(ccv_sim, SIM);
        _ ->
            print_run_error("dnn_ccv")
    end.


% run process to find similar objects by using the SURF
run_surf(Json) ->
    case jsonrpc:s(Json, surf) of
        undefiled ->
            print_run_error("dnn_surf");
        J ->
            run_surf_cmd(J)
    end.

run_surf_cmd(Json) ->
    case jsonrpc:s(Json, cmd) of
        Cmd when is_list(Cmd) ->
            run_surf_dbh(Json, Cmd);
        _ ->
            print_run_error("dnn_surf")
    end.

run_surf_dbh(Json, Cmd) ->
    case jsonrpc:s(Json, dbh) of
        DBH when is_list(DBH) ->
            run_surf_sim(Json, Cmd, DBH);
        _ ->
            print_run_error("dnn_surf")
    end.

run_surf_sim(Json, Cmd, DBH) ->
    case jsonrpc:s(Json, sim) of
        SIM when is_list(SIM) ->
            runcmd:start(surf, Cmd),
            runcmd:start(surf_dbh, DBH),
            runcmd:start(surf_sim, SIM);
        _ ->
            print_run_error("dnn_surf")
    end.
