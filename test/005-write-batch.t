#!/usr/bin/env escript

dbname() -> "test/dbs/005".
num_writes() -> 1000.

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    %timer:sleep(5000),
    etap:plan(4),
    os:cmd("rm -rf " ++ dbname()),
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    etap:is(test_simple_write(Db), ok, "Simple write batch is ok."),
    etap:is(test_add_rem(Db), ok, "Can add and remove keys in one batch."),
    etap:is(test_add_rem_same(Db), ok, "Can add and remove same key in batch."),
    etap:is(test_clear_batch(Db), ok, "Can clear a batch."),
    etap:is(test_large_batch(Db, num_writes()), ok, "Larger batch worked ok."),
    etap:end_tests().

test_simple_write(Db) ->
    {ok, Wb} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(Wb, <<"foo">>, <<"bar">>),
    ok = erleveldb:wb_put(Wb, <<"bing">>, <<"bang">>),
    ok = erleveldb:wb_write(Wb),
    {ok, <<"bar">>} = erleveldb:get(Db, <<"foo">>),
    {ok, <<"bang">>} = erleveldb:get(Db, <<"bing">>),
    ok.

test_add_rem(Db) ->
    {ok, Wb} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(Wb, "baz", <<"bam">>),
    ok = erleveldb:wb_del(Wb, "bing"),
    ok = erleveldb:wb_write(Wb),
    {ok, Iter} = erleveldb:iter(Db),
    {<<"baz">>, <<"bam">>} = erleveldb:seek(Iter, first),
    {<<"foo">>, <<"bar">>} = erleveldb:next(Iter),
    not_found = erleveldb:next(Iter),
    ok.

test_add_rem_same(Db) ->
    {ok, Wb} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(Wb, "bing", "bang"),
    ok = erleveldb:wb_del(Wb, "bing"),
    ok = erleveldb:wb_write(Wb),
    {error, not_found} = erleveldb:get(Db, "bing"),
    ok.
    
test_clear_batch(Db) ->
    {ok, Wb} = erleveldb:batch(Db),
    ok = erleveldb:wb_put(Wb, "zing", "zag"),
    ok = erleveldb:wb_clear(Wb),
    {error, not_found} = erleveldb:get(Db, "zing"),
    ok.

test_large_batch(Db, N) ->
    {ok, Wb} = erleveldb:batch(Db),
    test_large_batch(Db, Wb, N).

test_large_batch(Db, Wb, 0) ->
    ok = erleveldb:wb_write(Wb),
    lists:foreach(fun(N) ->
        K = to_key(N),
        {ok, K} = erleveldb:get(Db, K)
    end, lists:seq(1, num_writes())),
    ok;
test_large_batch(Db, Wb, N) ->
    ok = erleveldb:wb_put(Wb, to_key(N), to_key(N)),
    test_large_batch(Db, Wb, N-1).
    
to_key(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("~6.10.0b", [N])).