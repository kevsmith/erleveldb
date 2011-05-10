#!/usr/bin/env escript

dbname() -> "test/dbs/003".
num_writes() -> 1000. % Must be even

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(6),
    os:cmd("rm -rf " ++ dbname()),
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    etap:is(test_get_missing(Db), ok, "Missing key returns not_found."),
    etap:is(test_put_get_del(Db), ok, "Can put/get/del key/values."),
    etap:is(test_put_del_put_get(Db), ok, "Can overwrite deleted values."),
    etap:is(test_del_missing(Db), ok, "Deleting a missing key is ok."),
    etap:is(test_get_opts(Db), ok, "Can get keys with options set."),
    etap:is(test_many_updates(Db, num_writes()), ok, "Many updates were ok."),
    etap:end_tests().

test_get_missing(Db) ->
    {error, not_found} = erleveldb:get(Db, "foo"),
    ok.

test_put_get_del(Db) ->
    ok = erleveldb:put(Db, "foo", "bar"),
    {ok, <<"bar">>} = erleveldb:get(Db, "foo"),
    ok = erleveldb:del(Db, "foo"),
    {error, not_found} = erleveldb:get(Db, "foo"),
    ok.

test_del_missing(Db) ->
    ok = erleveldb:del(Db, "foo"),
    ok = erleveldb:del(Db, "foo"),
    ok.

test_put_del_put_get(Db) ->
    ok = erleveldb:put(Db, "foo", "bar"),
    ok = erleveldb:del(Db, "foo"),
    ok = erleveldb:put(Db, "foo", "baz"),
    {ok, <<"baz">>} = erleveldb:get(Db, "foo"),
    ok.

test_get_opts(Db) ->
    ok = erleveldb:put(Db, "foo", "baz"),
    {ok, <<"baz">>} = erleveldb:get(Db, "foo", []),
    {ok, <<"baz">>} = erleveldb:get(Db, "foo", [verify_checksums]),
    {ok, <<"baz">>} = erleveldb:get(Db, "foo", [skip_cache]),
    {ok, <<"baz">>} = erleveldb:get(Db, "foo", [verify_checksums, skip_cache]),
    ok = is_badarg(catch erleveldb:get(Db, "foo", [2])),
    ok = is_badarg(catch erleveldb:get(Db, "foo", [{foo,bar}])),
    ok.

is_badarg({'EXIT', {badarg, _}}) ->
    ok.

test_many_updates(Db, 0) ->
    test_delete_evens(Db, num_writes());
test_many_updates(Db, N) ->
    K = integer_to_list(N),
    ok = erleveldb:put(Db, K, K),
    test_many_updates(Db, N-1).

test_delete_evens(Db, 0) ->
    test_get_odds(Db, num_writes()-1);
test_delete_evens(Db, N) ->
    K = integer_to_list(N),
    ok = erleveldb:del(Db, K),
    test_delete_evens(Db, N-2).

test_get_odds(Db, 1) ->
    Resp = erleveldb:get(Db, <<"1">>),
    test_delete_odds(Db, num_writes()-1);
test_get_odds(Db, N) ->
    K = integer_to_list(N),
    V = list_to_binary(K),
    {ok, V} = erleveldb:get(Db, K),
    test_get_odds(Db, N-2).

test_delete_odds(Db, 1) ->
    ok = erleveldb:del(Db, <<"1">>),
    test_empty_db(Db, num_writes());
test_delete_odds(Db, N) ->
    K = integer_to_list(N),
    ok = erleveldb:del(Db, K),
    test_delete_odds(Db, N-2).

test_empty_db(_Db, 0) ->
    ok;
test_empty_db(Db, N) ->
    K = integer_to_list(num_writes() - N + 1),
    {error, not_found} = erleveldb:get(Db, K),
    test_empty_db(Db, N-1).
