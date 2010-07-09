-module(dnnsim).
-export([start/2, stop/0, get_similar_ccv/1, get_similar_surf/1]).

start(Dir, Home) ->
    DirAbs  = filename:absname(Dir),
    HomeAbs = filename:absname(Home),

    F1 = fun() ->
                 register(dnnsim_ccv, self()),
                 loop(ccv_sim, DirAbs, HomeAbs, ".ccv.hist.dbh")
         end,

    F2 = fun() ->
                 register(dnnsim_surf, self()),
                 loop(surf_sim, DirAbs, HomeAbs, ".surf.hist.dbh")
         end,

    spawn_link(F1),
    spawn_link(F2).

stop() ->
    dnnsim_ccv ! stop,
    dnnsim_surf ! stop.

get_similar_ccv(File) ->
    dnnsim_ccv ! {get, self(), File},
    receive
        {dnnsim, ccv_sim, Files} ->
            Files
    end.

get_similar_surf(File) ->
    dnnsim_surf ! {get, self(), File},
    receive
        {dnnsim, surf_sim, Files} ->
            Files
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Cmd, Dir, Home, Suffix) ->
    receive
        {get, PID, File} ->
            Files = get_similar(Cmd, Dir, Home, File, Suffix),
            PID ! {dnnsim, Cmd, Files},
            loop(Cmd, Dir, Home, Suffix);
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_similar(Cmd, Dir, Home, File, Suffix) ->
    DBH = [Dir, Home, File, Suffix],

    try runcmd:call_port(Cmd, "get") of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    try runcmd:call_port(Cmd, DBH) of
                        _ ->
                            get_similar(Cmd, [])
                    catch
                        _ ->
                            []
                    end
            end
    catch
        _ ->
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_similar(Cmd, Files) ->
    receive
        {Cmd, {eol, "."}} ->
            Files;
        {Cmd, {eol, File}} ->
            try runcmd:call_port(Cmd, "next") of
                _ ->
                    get_similar(Cmd, [File | Files])
            catch
                _ ->
                    Files
            end
    end.

