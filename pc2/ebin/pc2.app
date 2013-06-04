{application, pc2,
    [{description, "PC2 Bookmarking App 2.4"},
    {vsn, "2.4.0.1"},
    {modules,  [bookmarks_sup, bookmark_server]}, 
    {registered, [supervisor, bookmarks]},
    {applications, [kernel, stdlib]},
    {mod, {bookmarks_sup, ['pc@127.0.0.1']}}]}.
