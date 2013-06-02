-module(super).
-export([start_link/0, init/0, stop/0]).

init() ->
    %link to bookmark server
    process_flag(trap_exit, true),
    bookmark:start_link(),
    loop([]).

restart([]) ->
    bookmark:start_link(),
    loop([now()|[]]);
restart(N) when length(N) < 3 ->
    bookmark:start_link(),
    loop([now()|N]);
restart(N) ->
    Min = lists:min(N),
    Cur = now(),
    case timer:now_diff(Cur, Min) of
        T when (T > 3000000) ->
            bookmark:start_link(),
            loop([Cur|lists:delete(Min, N)]);
        _ -> 
            exit("Child won't recover")
    end.

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

