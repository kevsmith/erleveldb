#!/usr/bin/env escript

dbname() -> "test/dbs/002".

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(3),

    % Delete the db if it exists.
    os:cmd("rm -rf " ++ dbname()),

    etap:is(filelib:is_dir(dbname()), false, "Db directory does not exist."),

    etap:is(
        erleveldb:open_db(dbname()),
        {error, db_init_failed},
        "Db not created without create_if_missing"
    ),
    
    etap:is(test_open_db(), ok, "Db opened successfully."),
    true = erlang:garbage_collect(),
    etap:is(test_db_exists(), ok, "Db exists."),

    etap:is(filelib:is_dir(dbname()), true, "Db directory now exists."),
    
    etap:end_tests().

test_open_db() ->
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    ok.

test_db_exists() ->
    {error, db_init_failed} = erleveldb:open_db(dbname(), [error_if_exists]),
    ok.