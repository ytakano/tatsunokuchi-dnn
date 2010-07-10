-module(dnnimgs).
-export([start/3, stop/0, reload/0]).

-define(IMG_DIR, "/images").
-define(THUMB_DIR, "/thumbs").

start(Dir, Home, Hour) ->
    spawn_link(fun() ->
                       register(dnnimgs, self()),
                       init(Dir, Home),
                       loop(Dir, Home, Hour)
               end).

stop() ->
    dnnimgs ! stop.

reload() ->
    dnnimgs ! reload.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

                add_hash(ccv_sim,  FileRel, CCVDBH, CCVHIST),
                add_hash(surf_sim, FileRel, SURFDBH, SURFHIST),

                set_threshold(ccv_sim, 0.2),
                set_threshold(surf_sim, 0.01)
        end,

    Pat = "\\.jpeg$|\\.jpg$|\\.jpe$|\\.png$|\\.bmp$|\\.dib$|\\.tiff$|\\.tif$|\\.pbm$|\\.pgm$|\\.ppm$",%",

    filelib:fold_files([Home, ?IMG_DIR], Pat, true, F, []),
    dnnrndimgs:shuffle().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Dir, Home, Hour) ->
    F = fun(File, _) ->
                gen_thumb(File, Home),
                gen_ccv(File, Dir, Home),
                gen_surf(File, Dir, Home)
        end,

    Pat = "\\.jpeg$|\\.jpg$|\\.jpe$|\\.png$|\\.bmp$|\\.dib$|\\.tiff$|\\.tif$|\\.pbm$|\\.pgm$|\\.ppm$",%",

    filelib:fold_files([Home, ?IMG_DIR], Pat, true, F, []),

    receive
        reload ->
            loop(Dir, Home, Hour);
        stop ->
            ok
    after Hour * 60 * 60 * 1000 ->
            loop(Dir, Home, Hour)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_thumb(File, Home) ->
    FileRel = relative(filename:absname(File), filename:absname(Home)),

    Thumb = [Home, ?THUMB_DIR, FileRel, ".s.png"],

    FileTime  = filelib:last_modified(File),
    ThumbTime = filelib:last_modified(Thumb),

    if
        FileTime > ThumbTime ->
            try runcmd:call_port(resize, [File, "\n", Thumb, "\n"]) of
                _ ->
                    receive
                        {resize, {eol, "false"}} ->
                            false;
                        {resize, {eol, "true"}} ->
                            true
                    end
            catch
                _ ->
                    false
            end;
        true ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_ccv(File, Dir, Home) ->
    F1 = fun(Hist) ->
                 gen_dbh(ccv_dbh, Hist)
         end,

    F2 = fun(Str, Hash, Hist) ->
                 add_hash(ccv_sim, Str, Hash, Hist)
         end,

    gen_hist(ccv, File, Dir, Home, ".ccv.hist", F1, F2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_surf(File, Dir, Home) ->
    F1 = fun(Hist) ->
                 gen_dbh(surf_dbh, Hist)
         end,

    F2 = fun(Str, Hash, Hist) ->
                 add_hash(surf_sim, Str, Hash, Hist)
         end,

    gen_hist(surf, File, Dir, Home, ".surf.hist", F1, F2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_hist(Cmd, File, Dir, Home, Suffix, DBHFunc, SimFunc) ->
    FileAbs = filename:absname(File),
    DirAbs  = filename:absname(Dir),

    DBH  = [DirAbs, FileAbs, Suffix, ".dbh"],
    Hist = [DirAbs, FileAbs, Suffix],

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
                                    SimFunc(FileRel, Hash, Hist),
                                    dnnfiles:add(FileRel, FileTime);
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
relative([H1 | T1], [H2 | T2])
  when H1 == H2 ->
    relative(T1, T2);
relative(File, _) ->
    File.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_hash(Cmd, Str, Hash, Hist) ->
    try runcmd:call_port(Cmd, "add") of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    add_hash_str(Cmd, Str, Hash, Hist)
            end
    catch
        _ ->
            false
    end.

add_hash_str(Cmd, Str, Hash, Hist) ->
    try runcmd:call_port(Cmd, Str) of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    add_hash_file(Cmd, Hash, Hist)
            end
    catch
        _ ->
            false
    end.
    
add_hash_file(Cmd, Hash, Hist) ->
    try runcmd:call_port(Cmd, Hash) of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    add_hash_hist(Cmd, Hist)
            end
    catch
        _ ->
            false
    end.

add_hash_hist(Cmd, Hist) ->
    try runcmd:call_port(Cmd, Hist) of
        _ ->
            receive
                {Cmd, {eol, "true"}} ->
                    true;
                {Cmd, {eol, _}} ->
                    false
            end
    catch
        _ ->
            false
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_threshold(Cmd, Threshold) ->
    try runcmd:call_port(Cmd, "threshold") of
        _ ->
            receive
                {Cmd, {eol, _}} ->
                    F = io_lib:format("~p", [Threshold]),
                    try runcmd:call_port(Cmd, F) of
                        _ ->
                            receive
                                {Cmd, {eol, "true"}} ->
                                    true;
                                {Cmd, {eol, "false"}} ->
                                    false
                            end
                    catch
                        _ ->
                            false
                    end
            end
    catch
        _ ->
            false
    end.
