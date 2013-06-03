-module(bookmark).
-export([start_link/0, add_bookmark/1, add_bookmark/2, remove_bookmark/1,
        add_tag/2, remove_tag/2, get_bookmarks/0, get_bookmarks/1, stop/0, 
        connect_server/2]).
% Not in the spec, for convenience
-export([crash/1]).
% internal exports for spawning things
-export([client_init/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    % we don't want a random spawn, we should check whether bookmarks exist
    case whereis(bookmarks) of
        undefined -> register(bookmarks, 
                spawn_link(?MODULE, server_init, [])), {ok, whereis(bookmarks)};
        _Ref -> {error, already_started}
    end.

add_bookmark(Url) ->
    add_bookmark(Url, []).

add_bookmark(Url, Tags) -> 
    bookmarks ! {add, Url, Tags, self()},
    receive
        {addAck, Id} -> {ok, Id}
    end.

remove_bookmark(Id) -> 
    bookmarks ! {remove, Id}, ok.

add_tag(Id, Tag) -> 
    bookmarks ! {addtag, Id, Tag, self()},
    receive
        {addtagAck, Res} -> Res 
    end.

remove_tag(Id, Tag) -> 
    bookmarks ! {remtag, Id, Tag, self()},
    receive
        {remtagAck, Res} -> Res
    end.

get_bookmarks() -> 
    bookmarks ! {dump, self()},
    receive
        {got, Reply} -> Reply
    end.

get_bookmarks(Tags) -> 
    bookmarks ! {match, Tags, self()},
    receive
        {got, Reply} -> Reply
    end.

% Assuming here that the stop will kill the bookmark too.
stop() -> bookmarks ! stop, ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crash(Reason) -> bookmarks ! {crash, Reason}, ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Client process
client_init(Server, Backup) ->
    % monitor remote server
    monitor_node(Server, true),
    monitor_node(Backup, true),
    client_loop(Server, Backup).

client_loop(Server, Backup) ->
    receive 
        {nodedown, Server} -> io:format("Lost contact with bookmarking server\n"),
            client_recover(Server, Backup, Backup, Server, now());
        {nodedown, Backup} -> io:format("Lost contact with backup\n"), 
            client_recover(Server, Backup, Server, Backup, now());
        X -> {bookmarks, Server} ! X, client_loop(Server, Backup)
    end.

% Try to reconnect no more than every second
client_recover(Server, Backup, Up, Down, Timeout) ->
    case timer:now_diff (now(), Timeout) of
        T when (T > 1000000) ->
            monitor_node(Down, true),
            client_loop(Server, Backup);
        _ -> ok
    end,
    receive
        {nodedown, Up} -> io:format("Lost contact with both servers\n"), 
            exit("Lost contact with both servers\n");
        X -> {bookmarks, Up} ! X, 
            client_recover(Server, Backup, Up, Down, Timeout)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_server(Server, Backup) ->
    case whereis(bookmarks) of 
        undefined -> register(bookmarks, 
                spawn_link(?MODULE, client_init, [Server, Backup])), 
            {ok, whereis(bookmarks)};
        _Ref -> {error, already_connected}
    end.

