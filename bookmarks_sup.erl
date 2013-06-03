-module(bookmarks_sup).
-export([start/2, start_link/2, init/2, stop/0]).

init(Partner, Role) ->
    %link to bookmark server
    process_flag(trap_exit, true),
    bookmark_server:start_link(Partner, Role),
    loop([], Partner, Role).

restart([], Partner, Role) ->
    bookmark_server:start_link(),
    loop([now()|[]], Partner, Role);
restart(N, Partner, Role) when length(N) < 3 ->
    bookmark_server:start_link(),
    loop([now()|N], Partner, Role);
restart(N, Partner, Role) ->
    Min = lists:min(N),
    Cur = now(),
    case timer:now_diff(Cur, Min) of
        T when (T > 3000000) ->
            bookmark_server:start_link(),
            loop([Cur|lists:delete(Min, N)], Partner, Role);
        _ -> 
            exit("Child won't recover")
    end.

loop(N, Partner, Role) ->
    receive
        {stop} -> exit("stopped");
        {'EXIT', _Pid, _Reason} -> restart(N, Partner, Role)
    end.

start_link(Partner, Role) ->
    case whereis(supervisor) of
        undefined -> register(supervisor, spawn(?MODULE, init, [Partner, Role]));
        _Ref -> {error, already_started}
    end.

stop() -> 
    supervisor ! {stop}, ok.

start(Partner, Role) ->
    start_link(Partner, Role), io:format("Bookmark supervisor started\n").

