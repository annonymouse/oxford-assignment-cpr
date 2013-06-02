-module(super).
-export([start_link/0, init/0, stop/0]).

init() ->
    %link to bookmark server
    process_flag(trap_exit, true),
    bookmark:start_link(),
    loop(0).

restart(N) when N < 3 ->
    bookmark:start_link(),
    loop(N+1);
restart(_N) ->
    exit("Child restarted more than 3 times").

loop(N) ->
    receive
        {stop} -> exit("stopped");
        {'EXIT', _Pid, _Reason} -> restart(N)
    end.

start_link() ->
    case whereis(supervisor) of
        undefined -> register(supervisor, spawn(super, init, []));
        _Ref -> {error, already_started}
    end.

stop() -> 
    supervisor ! {stop}, ok.

