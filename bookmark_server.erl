-module(bookmark_server).
-export([start_link/0]).
% internal exports for spawning things
-export([server_init/0]).

server_init() -> 
    db_loop(ets:new(store, [set,private])).

add(DB, Url, Tags, Dest) ->
    Id = now(), 
    ets:insert(DB, {Id, Url, Tags}),
    Dest ! {addAck, Id},
    db_loop(DB).

remove(DB, Id) ->
    ets:delete(DB, Id),
    db_loop(DB).

update_tags(DB, Id, UpdateTags) ->
    case ets:lookup(DB, Id) of
        [{Id, Url, Tags}] -> ets:insert(DB, {Id, Url, UpdateTags(Tags)}), ok;
        [] -> {error, not_found}
    end.

tag(DB, Id, Tag, Dest) ->
    Dest ! {addtagAck, update_tags(DB, Id, fun(Tags) -> [Tag|Tags] end)},
    db_loop(DB).

untag(DB, Id, Tag, Dest) ->
    Dest ! {remtagAck, 
        update_tags(DB, Id, fun(Tags) -> lists:delete(Tags,Tag) end)},
    db_loop(DB).

dump(DB, Dest, Fun) ->
    Res = ets:foldl(Fun, [], DB),
    Dest ! {got, Res},
    db_loop(DB).

match_cmp(Tags) ->
    fun(Elem) -> lists:any(fun(InnerElem) -> Elem == InnerElem end, Tags) end.

make_matcher(Match) ->
    fun({_Id, Bookmark, Tags}, Acc) -> 
        case lists:all(match_cmp(Tags), Match) of
            true -> [Bookmark|Acc];
            false-> Acc
        end
    end.

db_loop(DB) -> 
    receive
        {add, Url, Tags, Dest} -> add(DB, Url, Tags, Dest);
        {remove, Id} -> remove(DB, Id);
        {addtag, Id, Tag, Dest} -> tag(DB, Id, Tag, Dest);
        {remtag, Id, Tag, Dest} -> untag(DB, Id, Tag, Dest);
        {dump, Dest} -> dump(DB, Dest, 
                fun({_Id, Bookmark,_Tags}, Acc) -> [Bookmark|Acc] end);
        {match, Match, Dest} -> dump(DB, Dest, make_matcher(Match)); 
        crash -> exit("Crash");
        stop -> ok
    end.

start_link() ->
    % we don't want a random spawn, we should check whether bookmarks exist
    case whereis(bookmarks) of
        undefined -> register(bookmarks, 
                spawn_link(?MODULE, server_init, [])), {ok, whereis(bookmarks)};
        _Ref -> {error, already_started}
    end.

