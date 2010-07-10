-module(runcmd).
-export([start/2, call_port/2, stop/1]).

start(Name, Cmd) ->
    F = fun() ->
                process_flag(trap_exit, true),
                Port = open_port({spawn, Cmd}, [{line, 4096}]),
                loop(Name, Port)
        end,
    PID = spawn_link(F),

    register(Name, PID).

call_port(Name, Msg) ->
    Name ! {call, self(), [Msg, "\n"]}.

stop(Name) ->
    Name ! stop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Name, Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, Msg}},

            receive
                {Port, {data, Data}} ->
                    Caller ! {Name, Data}
            end,

            Caller ! {Name, Msg},

            loop(Name, Port);
        stop ->
            Port ! {self(), close},

            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', _, _} ->
            exit({terminated, Name});
        {Port, {data, Data}} ->
            io:format("~p: ~p\n", [Name, Data]),
            loop(Name, Port)
    end.
