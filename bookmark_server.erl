-module(bookmark_server).
-export([start_link/2]).
% internal exports for spawning things
-export([server_init/2]).

server_init(Partner, Role) -> 
    {bookmarks , Partner} ! {init, self(), Role},
    db_loop(ets:new(store, [set,private]), Partner).

add(DB, Partner, Url, Tags, Dest) ->
    Id = now(), 
    ets:insert(DB, {Id, Url, Tags}),
    Dest ! {addAck, Id},
    db_loop(DB, Partner).

remove(DB, Partner, Id) ->
    ets:delete(DB, Id),
    db_loop(DB, Partner).

update_tags(DB, Id, UpdateTags) ->
    case ets:lookup(DB, Id) of
        [{Id, Url, Tags}] -> ets:insert(DB, {Id, Url, UpdateTags(Tags)}), ok;
        [] -> {error, not_found}
    end.

tag(DB, Partner, Id, Tag, Dest) ->
    Dest ! {addtagAck, update_tags(DB, Id, fun(Tags) -> [Tag|Tags] end)},
    db_loop(DB, Partner).

untag(DB, Partner, Id, Tag, Dest) ->
    Dest ! {remtagAck, 
        update_tags(DB, Id, fun(Tags) -> lists:delete(Tags,Tag) end)},
    db_loop(DB, Partner).

dump(DB, Partner, Dest, Fun) ->
    Res = ets:foldl(Fun, [], DB),
    Dest ! {got, Res},
    db_loop(DB, Partner).

match_cmp(Tags) ->
    fun(Elem) -> lists:any(fun(InnerElem) -> Elem == InnerElem end, Tags) end.

make_matcher(Match) ->
    fun({_Id, Bookmark, Tags}, Acc) -> 
        case lists:all(match_cmp(Tags), Match) of
            true -> [Bookmark|Acc];
            false-> Acc
        end
    end.

partner_awake(DB, Partner, Role) ->
    % Partner has woken up, we should share state
    monitor_node(Partner, true),
    case Role of 
        slave -> {bookmark, Partner} ! {flatten, DB};
        _ -> ok
    end,
    db_loop(DB, Partner).
    
db_loop(DB, Partner) -> 
    receive
        {add, Url, Tags, Dest} -> add(DB, Partner,  Url, Tags, Dest);
        {remove, Id} -> remove(DB, Partner, Id);
        {addtag, Id, Tag, Dest} -> tag(DB, Partner, Id, Tag, Dest);
        {remtag, Id, Tag, Dest} -> untag(DB, Partner, Id, Tag, Dest);
        {dump, Dest} -> dump(DB, Partner, Dest, 
                fun({_Id, Bookmark,_Tags}, Acc) -> [Bookmark|Acc] end);
        {match, Match, Dest} -> dump(DB, Partner, Dest, make_matcher(Match)); 
        {flatten, OtherDB} when is_list(OtherDB) -> db_loop(OtherDB, Partner);
        {init, Partner, Role} -> partner_awake(DB, Partner, Role);
        {nodedown, Partner} -> db_loop(DB, Partner);
        {crash, Reason} -> exit(Reason);
        stop -> ok
    end.

start_link(Partner, Role) ->
    % we don't want a random spawn, we should check whether bookmarks exist
    case whereis(bookmarks) of
        undefined -> register(bookmarks, 
                spawn_link(?MODULE, server_init, [Partner, Role])),
            {ok, whereis(bookmarks)};
        _Ref -> {error, already_started}
    end.

