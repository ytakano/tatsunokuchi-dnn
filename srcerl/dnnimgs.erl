-module(dnnimgs).
-export([gen_ccv/3, gen_surf/3]).

gen_ccv(File, Dir, Home) ->
    F1 = fun(Hist) ->
                 gen_dbh(ccv_dbh, Hist)
         end,

    F2 = fun(Str, Hash) ->
                 add_hash(ccv_sim, Str, Hash)
         end,

    gen_hist(ccv, File, Dir, Home, ".ccv.hist.dbh", F1, F2).

gen_surf(File, Dir, Home) ->
    F1 = fun(Hist) ->
                 gen_dbh(surf_dbh, Hist)
         end,

    F2 = fun(Str, Hash) ->
                 add_hash(surf_sim, Str, Hash)
         end,

    gen_hist(surf, File, Dir, Home, ".ccv.surf.dbh", F1, F2).

gen_hist(Cmd, File, Dir, Home, Suffix, DBHFunc, SimFunc) ->
    FileAbs = filename:absname(File),
    DirAbs  = filename:absname(Dir),

    DBH  = [DirAbs, FileAbs, Suffix],

    FileTime = filelib:last_modified(File),
    DBHTime  = filelib:last_modified(DBH),

    if
        FileTime > DBHTime ->
            try runcmd:call_port(Cmd, File) of
                _ ->
                    receive
                        {Cmd, {eol, "false"}} ->
                            false;
                        {Cmd, {eol, Line}} ->
                            case DBHFunc(Line) of
                                {ok, Hash} ->
                                    FileRel = relative(FileAbs,
                                                       filename:absname(Home)),
                                    SimFunc(FileRel, Hash);
                                _ ->
                                    false
                            end
                    end
            catch
                _ ->
                    false
            end;
        true ->
            ok
    end.

gen_dbh(Cmd, Hist) ->
    try runcmd:call_port(Cmd, Hist) of
        _ ->
            receive
                {Cmd, {eol, "false"}} ->
                    false;
                {Cmd, {eol, Line}} ->
                    {ok, Line}
            end
    catch
        _ ->
            false
    end.

relative([H1 | T1], [H2 | T2])
  when H1 == H2 ->
    relative(T1, T2);
relative(File, _) ->
    File.

add_hash(Cmd, File, Hash) ->
    try runcmd:call_port(Cmd, ["add\n", File, "\n", Hash]) of
        _ ->
            receive
                {Cmd, {eol, "false"}} ->
                    false;
                {Cmd, {eol, Line}} ->
                    io:format("~s\n", [Line]),
                    ok
            end
    catch
        _ ->
            false
    end.
