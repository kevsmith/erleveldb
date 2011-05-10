#!/usr/bin/env escript

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(1),
    etap:loaded_ok(erleveldb, "Loaded: erleveldb"),
    etap:end_tests().
