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

dump(DB, Dest, Fun) ->
    Res = ets:foldl(Fun, [], DB),
    Dest ! {got, Res},
    db_loop(DB).

make_search(Match) ->
    fun (Elem) ->
            lists:any(fun(IElem) -> Elem == IElem end, Match) end.

make_matcher(Match) ->
    % We're doing a crap solution here, TODO make this faster by sorting 
    % list patterns.
    fun({_Id, Bookmark, Tags}, Acc) ->
            case lists:any(make_search(Match), Tags) of 
                true -> [Bookmark|Acc];
                false -> Acc
            end
    end.

db_loop(DB) -> 
    receive
        {add, Url, Tags, Dest} -> add(DB, Url, Tags, Dest);
        {remove, Id} -> remove(DB, Id);
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

add_tag(_Id, _Tag) -> {error, not_found}.

remove_tag(_Id, _Tag) -> {error, not_implemented}.

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

