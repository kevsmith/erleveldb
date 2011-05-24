#!/usr/bin/env escript
%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.

dbname() -> "test/dbs/002".
dbname2() -> "test/dbs/002-destroy".

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(8),

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
    etap:is(test_bad_option(), ok, "Db errors with bad options."),
    etap:is(filelib:is_dir(dbname()), true, "Db directory now exists."),
    
    etap:is(test_destroy_db(), ok, "Db destroyed successfully."),
    erlang:garbage_collect(),
    etap:is(filelib:is_dir(dbname2()), false, "Db is in fact, gone."),

    etap:end_tests().

test_open_db() ->
    {ok, Db} = erleveldb:open_db(dbname(), [create_if_missing]),
    ok.

test_db_exists() ->
    {error, db_init_failed} = erleveldb:open_db(dbname(), [error_if_exists]),
    ok.

test_bad_option() ->
    ok = is_badarg(catch erleveldb:open_db(dbname(), [foo])),
    ok = is_badarg(catch erleveldb:open_db(dbname(), [{write_buffer_size, f}])),
    ok.

test_destroy_db() ->
    {ok, Db} = erleveldb:open_db(dbname2(), [create_if_missing]),
    true = filelib:is_dir(dbname2()),
    ok = erleveldb:destroy_db(Db),
    ok.

is_badarg({'EXIT', {badarg, _}}) ->
    ok.
