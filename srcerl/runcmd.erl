-module(runcmd).
-export([start/2, call_port/2, stop/1, call_port_null/1]).

start(Name, Cmd) ->
    F = fun() ->
                register(Name, self()),
                process_flag(trap_exit, true),
                Port = open_port({spawn, Cmd}, [{line, 4096}]),
                loop(Name, Port)
        end,
    spawn_link(F).

call_port(Name, Msg) ->
    Name ! {call, self(), [Msg, "\n"]}.

call_port_null(Name) ->
    Name ! {call, self(), ""}.

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
        W ->
            io:format("????: ~p\n", W),
            loop(Name, Port)
    end.
