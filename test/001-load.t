#!/usr/bin/env escript
%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.


main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    etap:plan(1),
    etap:loaded_ok(erleveldb, "Loaded: erleveldb"),
    etap:end_tests().
