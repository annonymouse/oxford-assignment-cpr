-module(super).
-export([start_link/0, init/0, stop/0]).

init() ->
    %link to bookmark server
    loop(register(bookmarks, spawn_link(bookmark, init, []))).

loop(_Child) ->
    receive
        {stop} -> ok
    end.

start_link() ->
    case whereis(supervisor) of
        undefined -> register(supervisor, spawn(super, init, []));
        _Ref -> {error, already_started}
    end.

stop() -> 
    supervisor ! {stop}, ok.

