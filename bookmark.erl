-module(bookmark).
-export([start_link/0, add_bookmark/1, add_bookmark/2, remove_bookmark/1,
        add_tag/2, remove_tag/2, get_bookmarks/0, get_bookmarks/1, stop/0]).
% Not in the spec, for convenience
-export([]).
% internal exports for spawning things
-export([init/0]).

init() -> 
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
        stop -> ok
    end.
    
start_link() ->
    % we don't want a random spawn, we should check whether bookmarks exist
    case whereis(bookmarks) of
        undefined -> register(bookmarks, 
                spawn(?MODULE, init, [])), {ok, whereis(bookmarks)};
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

% get is a bif, so we can't use it as an atom here...
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

