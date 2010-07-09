-module(dnnrndimgs).
-export([start/0, get_imgs/0, stop/0, shuffle/0]).

-define(RND_ARR, [1, 2, 3, 5, 7, 11, 13, 17, 19, 23]).

start() ->
    F = fun() ->
                register(dnnrndimgs, self()),
                loop()
        end,

    spawn_link(F).

get_imgs() ->
    dnnrndimgs ! {get, self()},
    receive
        {dnnrndimgs, Files} ->
            Files
    end.

stop() ->
    dnnrndimgs ! stop.

shuffle() ->
    dnnrndimgs ! shuffle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop() ->
    skip_imgs(?RND_ARR),
    receive
        {get, PID} ->
            PID ! {dnnrndimgs, get_imgs(?RND_ARR, [])},
            loop();
        shuffle ->
            loop();
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_imgs([], Files) ->
    Files;
get_imgs([H | T], Files) ->
    case get(H) of
        undefined ->
            get_imgs(T, Files);
        F ->
            get_imgs(T, [F | Files])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
