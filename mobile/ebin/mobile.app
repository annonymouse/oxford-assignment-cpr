{application, mobile,
    [{description, "Mobile Bookmarking Clients 2.4"},
    {vsn, "2.4.0.1"},
    {modules,  [bookmark]}, 
    {registered, [bookmarks]},
    {applications, [kernel, stdlib]},
    {mod, {bookmark, ['pc@127.0.0.1', 'pc2@127.0.0.1']}}]}.
