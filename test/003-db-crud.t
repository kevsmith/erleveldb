#!/usr/bin/env escript

dbname() -> "test/dbs/003".
num_writes() -> 1000. % Must be even

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(11),
    os:cmd("rm -rf " ++ dbname()),
    {ok, Db} = erleveldb:open_db(dbname()),
    ok = test_get_missing(Db),
    ok = test_put_get_del(Db),
    ok = test_put_del_put_get(Db),
    ok = test_del_missing(Db),
    etap:is(test_many_updates(Db, num_writes()), ok, "Many updates were ok."),
    etap:end_tests().

test_get_missing(Db) ->
    etap:is(erleveldb:get(Db, "foo"), {error, not_found}, "Get k is not_found"),
    ok.

test_put_get_del(Db) ->
    etap:is(erleveldb:put(Db, "foo", "bar"), ok, "Put k/v is ok."),
    etap:is(erleveldb:get(Db, "foo"), {ok, <<"bar">>}, "Get k returns v."),
    etap:is(erleveldb:del(Db, "foo"), ok, "Del k returns ok."),
    etap:is(erleveldb:get(Db, "foo"), {error, not_found}, "Get k is not_found"),
    ok.

test_del_missing(Db) ->
    etap:is(erleveldb:del(Db, "foo"), ok, "Del missing is not an error."),
    ok.

test_put_del_put_get(Db) ->
    etap:is(erleveldb:put(Db, "foo", "bar"), ok, "Put k/v is ok."),
    etap:is(erleveldb:del(Db, "foo"), ok, "Del k is ok."),
    etap:is(erleveldb:put(Db, "foo", "baz"), ok, "Re-put k/v is ok."),
    etap:is(erleveldb:get(Db, "foo"), {ok, <<"baz">>}, "Got new v ok."),
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
