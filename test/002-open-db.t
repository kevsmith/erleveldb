#!/usr/bin/env escript

dbname() -> "test/dbs/002".

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(3),

    % Delete the db if it exists.
    os:cmd("rm -rf " ++ dbname()),

    etap:is(filelib:is_dir(dbname()), false, "Db directory does not exist."),

    Resp = erleveldb:open_db(dbname()),
    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        Resp,
        "Db opened successfully."
    ),

    etap:is(filelib:is_dir(dbname()), true, "Db directory now exists."),
    
    etap:end_tests().
