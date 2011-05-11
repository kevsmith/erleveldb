#!/usr/bin/env escript

dbname() -> "test/dbs/004".
num_writes() -> 1000.

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(5),
    os:cmd("rm -rf " ++ dbname()),
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    etap:is(test_write_kvs(Db, num_writes()), ok, "Wrote k/v pairs."),
    etap:is(test_seek_forward(Db), ok, "Seeked forward."),
    etap:is(test_seek_reverse(Db), ok, "Seeked reverse."),
    etap:is(test_seek_key(Db), ok, "Seeked to key."),
    etap:is(test_iter_opts(Db), ok, "Iterators can be created with options."),
    etap:end_tests().

test_write_kvs(_Db, 0) ->
    ok;
test_write_kvs(Db, N) ->
    K = to_key(N),
    ok = erleveldb:put(Db, K, K),
    test_write_kvs(Db, N-1).

test_seek_forward(Db) ->
    {ok, Iter} = erleveldb:iter(Db),
    {<<"000001">>, <<"000001">>} = erleveldb:seek(Iter, first),
    lists:foreach(fun(N) ->
        Next = {to_key(N), to_key(N)},
        Next = erleveldb:next(Iter)
    end, lists:seq(2, num_writes())),
    not_found = erleveldb:next(Iter),
    ok.

test_seek_reverse(Db) ->
    {ok, Iter} = erleveldb:iter(Db),
    {<<"001000">>, <<"001000">>} = erleveldb:seek(Iter, last),
    lists:foreach(fun(N) ->
        Next = {to_key(N), to_key(N)},
        Next = erleveldb:prev(Iter)
    end, lists:seq(num_writes()-1, 1, -1)),
    not_found = erleveldb:prev(Iter),
    ok.

test_seek_key(Db) ->
    {ok, Iter} = erleveldb:iter(Db),
    lists:foreach(fun(N) ->
        Next = {to_key(N), to_key(N)},
        Next = erleveldb:seek(Iter, to_key(N))
    end, lists:seq(1, num_writes(), 5)),
    ok.

test_iter_opts(Db) ->
    {ok, _} = erleveldb:iter(Db, []),
    {ok, _} = erleveldb:iter(Db, [verify_checksums]),
    {ok, _} = erleveldb:iter(Db, [skip_cache]),
    {ok, _} = erleveldb:iter(Db, [verify_checksums, skip_cache]),
    ok = is_badarg(catch erleveldb:iter(Db, bing)),
    ok = is_badarg(catch erleveldb:iter(Db, [2])),
    ok = is_badarg(catch erleveldb:iter(Db, [{foo,bar}])),
    ok.

to_key(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("~6.10.0b", [N])).

is_badarg({'EXIT', {badarg, _}}) ->
    ok.
