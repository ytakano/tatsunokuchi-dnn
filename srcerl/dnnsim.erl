-module(dnnsim).
-export([start/2, stop/0, get_similar_ccv/1, get_similar_surf/1]).
-export([clear_cache_ccv/0, clear_cache_surf/0]).

start(Dir, Home) ->
    DirAbs  = filename:absname(Dir),
    HomeAbs = filename:absname(Home),

    F1 = fun() ->
                 TID = ets:new(ccv_sim_cache, [set]),
                 register(dnnsim_ccv, self()),
                 loop(ccv_sim, DirAbs, HomeAbs, ".ccv.hist", TID)
         end,

    F2 = fun() ->
                 TID = ets:new(surf_sim_cache, [set]),
                 register(dnnsim_surf, self()),
                 loop(surf_sim, DirAbs, HomeAbs, ".surf.hist", TID)
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

clear_cache_ccv() ->
    dnnsim_ccv ! clear_cache.

clear_cache_surf() ->
    dnnsim_surf ! clear_cache.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Cmd, Dir, Home, Suffix, TID) ->
    receive
        {get, PID, File} ->
            Files = case ets:lookup(TID, File) of
                        [{File, F}] ->
                            F;
                        _ ->
                            F = get_similar(Cmd, Dir, Home, File, Suffix),
                            ets:insert(TID, {File, F}),
                            F
                    end,

            PID ! {dnnsim, Cmd, Files},

            loop(Cmd, Dir, Home, Suffix, TID);
        clear_cache ->
            ets:delete_all_objects(TID),
            loop(Cmd, Dir, Home, Suffix, TID);
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_similar(Cmd, Dir, Home, File, Suffix) ->
    Hist = [Dir, Home, File, Suffix],
    DBH  = [Dir, Home, File, Suffix, ".dbh"],

    try runcmd:call_port(Cmd, "get") of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    try runcmd:call_port(Cmd, DBH) of
                        _ ->
                            receive
                                {Cmd, {eol, _}} ->
                                    try runcmd:call_port(Cmd, Hist) of
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
            lists:reverse(Files);
        {Cmd, {eol, File}} ->
            try runcmd:call_port(Cmd, "next") of
                _ ->
                    get_similar(Cmd, [File | Files])
            catch
                _ ->
                    lists:reverse(Files)
            end
    end.

