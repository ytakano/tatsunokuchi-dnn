-module(dnnfiles).
-export([start/0, stop/0, add/2, remove/1]).

start() ->
    spawn_link(fun init/0).

add(File, Date) ->
    dnnfiles ! {add, File, Date}.

remove(File) ->
    dnnfiles ! {remove, File}.

stop() ->
    dnnfiles ! stop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    register(dnnfiles, self()),

    ets:new(files, [ordered_set, named_table]),
    ets:new(dates, [ordered_set, named_table]),

    loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop() ->
    receive
        {add, File, Date} ->
            add_file(File, Date),
            loop();
        {remove, File} ->
            remove_file(File),
            loop();
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_file(File) ->
    case ets:lookup(files, File) of
        [{File, Date}] ->
            rm_by_date(File, Date),
            ets:delete(files, File);
        _ ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm_by_date(File, Date) ->
    case ets:lookup(dates, Date) of
        [{Date, Files}] ->
            F = sets:del_element(File, Files),
            case sets:size(F) of
                S when S > 0 ->
                    ets:insert(dates, {Date, F});
                _ ->
                    ets:delete(dates, Date)
            end;
        _ ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_file(File, Date) ->
    remove_file(File),

    ets:insert(files, {File, Date}),

    F = case ets:lookup(dates, Date) of
            [{Date, Files}] ->
                sets:add_element(File, Files);
            _ ->
                sets:add_element(File, sets:new())
        end,

    ets:insert(dates, {Date, F}).
