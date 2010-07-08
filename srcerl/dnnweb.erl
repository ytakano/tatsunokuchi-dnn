-module(dnnweb).
-export([start/1]).

start([Conf]) ->
    process_flag(trap_exit, true),
    open_json(Conf),
    loop().

loop() ->
    receive
        {ccv, Line} ->
            io:format("ccv: ~p\n", [Line]),
            loop();
        {'EXIT', _, Reason} ->
            yaws:stop(),
            io:format("stopped: ~p\n", [Reason]),
            init:stop()
    end.

open_json(Conf) ->
    try file:read_file(Conf) of
        {ok, Str} ->
            io:format("~p\n", [Str]),
            read_json(Str);
        _ ->
            io:format("failed to open \"~p\"", [Conf])
    catch
        _:W ->
            io:format("internal error: ~p\n", [W])
    end.

read_json(Str) ->
    try json:decode_string(binary_to_list(Str)) of
        {ok, Json} ->
            run_web(Json),
            run_ccv(Json);
        _ ->
            io:format("invalid config file\n")
    catch
        _:W ->
            io:format("internal error: ~p\n", [W])
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
                   7200
           end,

    case jsonrpc:s(Json, logdir) of
        Dir when is_list(Dir) ->
            yaws:start_embedded(Home, [{port, Port}], [{logdir, Dir}]);
        _ ->
            yaws:start_embedded(Home, [{port, Port}])
    end.


run_ccv(Json) ->
    Cmd = case jsonrpc:s(Json, ccv) of
              C when is_list(C) ->
                  C;
              _ ->
                   "ccv"
          end,

    runcmd:start(ccv, Cmd).
