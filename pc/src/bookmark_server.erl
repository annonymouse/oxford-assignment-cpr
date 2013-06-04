-module(bookmark_server).
-export([start_link/1]).
% internal exports for spawning things
-export([server_init/1]).

server_init(Partner) -> 
    {bookmarks , Partner} ! {init, node()},
    db_loop(ets:new(store, [set,private]), Partner).

add(DB, Partner, Url, Tags, Dest) ->
    Id = now(), 
    {bookmarks, Partner} ! {sync_add, {Id, Url, Tags}},
    ets:insert(DB, {Id, Url, Tags}),
    Dest ! {addAck, Id},
    db_loop(DB, Partner).

remove(DB, Partner, Id) ->
    {bookmarks, Partner} ! {sync_rem, Id},
    ets:delete(DB, Id),
    db_loop(DB, Partner).

update_tags(DB, Id, UpdateTags) ->
    case ets:lookup(DB, Id) of
        [{Id, Url, Tags}] -> ets:insert(DB, {Id, Url, UpdateTags(Tags)}), ok;
        [] -> {error, not_found}
    end.

tag(DB, Partner, Id, Tag, Dest) ->
    {bookmarks, Partner} ! {sync_addtag, Id, Tag},
    Dest ! {addtagAck, update_tags(DB, Id, fun(Tags) -> [Tag|Tags] end)},
    db_loop(DB, Partner).

untag(DB, Partner, Id, Tag, Dest) ->
    {bookmarks, Partner} ! {sync_remtag, Id, Tag},
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

sync_add(DB, Partner, Data) ->
    ets:insert(DB, Data),
    db_loop(DB, Partner).

sync_rem(DB, Partner, Id) ->
    ets:delete(DB, Id),
    db_loop(DB, Partner).

sync_addtag(DB, Partner, Id, Tag) ->
    update_tags(DB, Id, fun(Tags) -> [Tag|Tags] end), 
    db_loop(DB, Partner).

sync_removetag(DB, Partner, Id, Tag) ->
    update_tags(DB, Id, fun(Tags) -> lists:delete(Tags, Tag) end),
    db_loop(DB, Partner).

partner_awake(DB, Partner) ->
    % Partner has woken up, we should share state
    monitor_node(Partner, true),
    {bookmarks, Partner} ! {flatten, ets:tab2list(DB)},
    db_loop(DB, Partner).

overwrite(DB, OtherDB, Partner) ->
    lists:foreach(fun(Elem) -> ets:insert(Elem), Elem end, OtherDB),
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
        %partner bookmark server messages
        {flatten, OtherDB} when is_list(OtherDB) -> 
            overwrite(DB, OtherDB, Partner);
        {init, NewPartner} -> partner_awake(DB, NewPartner);
        {nodedown, Partner} -> db_loop(DB, Partner);
        {sync_add, Data} -> sync_add(DB, Partner, Data);
        {sync_rem, Id} -> sync_rem(DB, Partner, Id);
        {sync_addtag, Id, Tag} -> sync_addtag(DB, Partner, Id, Tag);
        {sync_remtag, Id, Tag} -> sync_removetag(DB, Partner, Id, Tag);
        {debug} -> tv:start(), db_loop(DB, Partner);
        {crash, Reason} -> exit(Reason);
        stop -> ok
    end.

start_link(Partner) ->
    % we don't want a random spawn, we should check whether bookmarks exist
    case whereis(bookmarks) of
        undefined -> register(bookmarks, 
                spawn_link(?MODULE, server_init, [Partner])),
            {ok, whereis(bookmarks)};
        _Ref -> {error, already_started}
    end.

