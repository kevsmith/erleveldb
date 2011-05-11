#!/usr/bin/env escript

dbname() -> "test/dbs/006".

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(unknown),
    os:cmd("rm -rf " ++ dbname()),
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    etap:is(test_basic_snapshot(Db), ok, "Basic snapshots work."),
    etap:is(test_multi_snapshot(Db), ok, "Multiple snapshots work."),
    etap:is(test_iter_snapshot(Db), ok, "Iterators work with snapshots."),
    etap:is(test_put_snapshot(Db), ok, "Snapshot from put work."),
    etap:is(test_del_snapshot(Db), ok, "Snapshot from del work."),
    etap:is(test_batch_snapshot(Db), ok, "Snapshots from batch writes work."),
    etap:end_tests().

test_basic_snapshot(Db) ->
    {ok, Snap} = erleveldb:snapshot(Db),
    ok = erleveldb:put(Db, "01", "01"),
    {ok, <<"01">>} = erleveldb:get(Db, "01"),
    {error, not_found} = erleveldb:get(Db, "01", [{snapshot, Snap}]),
    ok.

test_multi_snapshot(Db) ->
    {ok, Snap1} = erleveldb:snapshot(Db),
    ok = erleveldb:put(Db, "02", "02"),
    {ok, Snap2} = erleveldb:snapshot(Db),
    ok = erleveldb:put(Db, "03", "03"),
    {ok, <<"03">>} = erleveldb:get(Db, "03"),

    {ok, <<"01">>} = erleveldb:get(Db, "01", [{snapshot, Snap1}]),
    {error, not_found} = erleveldb:get(Db, "02", [{snapshot, Snap1}]),    
    {error, not_found} = erleveldb:get(Db, "03", [{snapshot, Snap1}]),

    {ok, <<"01">>} = erleveldb:get(Db, "01", [{snapshot, Snap2}]),
    {ok, <<"02">>} = erleveldb:get(Db, "02", [{snapshot, Snap2}]),    
    {error, not_found} = erleveldb:get(Db, "03", [{snapshot, Snap2}]),

    ok.

test_iter_snapshot(Db) ->
    {ok, Snap} = erleveldb:snapshot(Db),
    {ok, Iter1} = erleveldb:iter(Db, [{snapshot, Snap}]),
    ok = erleveldb:put(Db, "04", "04"),
    ok = erleveldb:put(Db, "05", "05"),
    {<<"01">>, <<"01">>} = erleveldb:seek(Iter1, first),
    {<<"02">>, <<"02">>} = erleveldb:next(Iter1),
    {<<"03">>, <<"03">>} = erleveldb:next(Iter1),
    {<<"02">>, <<"02">>} = erleveldb:prev(Iter1),
    {<<"03">>, <<"03">>} = erleveldb:next(Iter1),
    not_found = erleveldb:next(Iter1),
    {ok, Iter2} = erleveldb:iter(Db),
    {<<"01">>, <<"01">>} = erleveldb:seek(Iter2, first),
    lists:foreach(fun(N) ->
        Next = {to_key(N), to_key(N)},
        Next = erleveldb:next(Iter2)
    end, lists:seq(2, 5)),
    not_found = erleveldb:next(Iter2),
    ok.

test_put_snapshot(Db) ->
    {ok, Snap} = erleveldb:put(Db, "06", "06", [snapshot]),
    ok = erleveldb:put(Db, "07", "07"),
    {ok, <<"06">>} = erleveldb:get(Db, "06", [{snapshot, Snap}]),
    {error, not_found} = erleveldb:get(Db, "07", [{snapshot, Snap}]),
    ok.

test_del_snapshot(Db) ->
    {ok, Snap} = erleveldb:del(Db, "07", [snapshot]),
    ok = erleveldb:del(Db, "06"),
    {error, not_found} = erleveldb:get(Db, "07", [{snapshot, Snap}]),
    {ok, <<"06">>} = erleveldb:get(Db, "06", [{snapshot, Snap}]),
    ok.

test_batch_snapshot(Db) ->
    {ok, WB1} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(WB1, "08", "08"),
    ok = erleveldb:wb_del(WB1, "05"),
    
    {ok, Snap} = erleveldb:wb_write(WB1, [snapshot]),
    
    {ok, WB2} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(WB2, "09", "09"),
    ok = erleveldb:wb_del(WB2, "04"),
    ok = erleveldb:wb_write(WB2),

    {ok, <<"08">>} = erleveldb:get(Db, "08", [{snapshot, Snap}]),
    {error, not_found} = erleveldb:get(Db, "05", [{snapshot, Snap}]),
    {error, not_found} = erleveldb:get(Db, "09", [{snapshot, Snap}]),
    {ok, <<"04">>} = erleveldb:get(Db, "04", [{snapshot, Snap}]),
    
    ok.

to_key(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("~2.10.0b", [N])).

