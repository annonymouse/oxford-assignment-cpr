-module(bookmarks_sup).
-export([start/1, start_link/1, init/1, stop/0]).

init(Partner) ->
    %link to bookmark server
    process_flag(trap_exit, true),
    bookmark_server:start_link(Partner),
    loop([], Partner).

restart([], Partner) ->
    bookmark_server:start_link(Partner),
    loop([now()|[]], Partner);
restart(N, Partner) when length(N) < 3 ->
    bookmark_server:start_link(Partner),
    loop([now()|N], Partner);
restart(N, Partner) ->
    Min = lists:min(N),
    Cur = now(),
    case timer:now_diff(Cur, Min) of
        T when (T > 3000000) ->
            bookmark_server:start_link(Partner),
            loop([Cur|lists:delete(Min, N)], Partner);
        _ -> 
            exit("Child won't recover")
    end.

loop(N, Partner) ->
    receive
        {stop} -> exit("stopped");
        {'EXIT', _Pid, _Reason} -> restart(N, Partner)
    end.

start_link(Partner) ->
    case whereis(supervisor) of
        undefined -> register(supervisor, spawn(?MODULE, init, [Partner]));
        _Ref -> {error, already_started}
    end.

stop() -> 
    supervisor ! {stop}, ok.

start([Partner]) ->
    start_link(Partner), io:format("Bookmark supervisor started\n").

