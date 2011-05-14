%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.
%%
%% @author Paul J. Davis <paul.joseph.davis@gmail.com>
%% @copyright 2011 Paul J. Davis
%% @version 0.1.0
%% @reference <a href="http://leveldb.googlecode.com">LevelDB</a> 
%%

-module(erleveldb).
-on_load(init/0).


-include("erleveldb.hrl").


-define(i2b(B), iolist_to_binary(B)).
-define(location, [{module, ?MODULE}, {line, ?LINE}]).
-define(not_loaded, erlang:nif_error(not_loaded, [?location])).


-export([open_db/1, open_db/2]).
-export([put/3, put/4, get/2, get/3, del/2, del/3]).
-export([iter/1, iter/2, seek/2, next/1, prev/1]).
-export([batch/1, wb_put/3, wb_del/2, wb_clear/1, wb_write/1, wb_write/2]).
-export([snapshot/1]).


-export_type([db/0, iterator/0, write_batch/0, snapshot/0]).
-export_type([dbname/0, dbopts/0, readopts/0, writeopts/0]).
-export_type([ikey/0, ival/0, key/0, val/0, seek_dest/0]).


-spec open_db(dbname()) -> {ok, db()} | error().
open_db(_Name) ->
    ?not_loaded.

-spec open_db(dbname(), dbopts()) -> {ok, db()} | error().
open_db(_Name, _Opts) ->
    ?not_loaded.


-spec put(db(), ikey(), ival()) -> ok | error().
put(_Db, _Key, _Value) ->
    ?not_loaded.

-spec put(db(), ikey(), ival(), writeopts()) -> ok | {ok, val()} | error().
put(_Db, _Key, _Value, _Opts) ->
    ?not_loaded.


-spec get(db(), ikey()) -> {ok, val()} | error().
get(_Db, _Key) ->
    ?not_loaded.

-spec get(db(), ikey(), readopts()) -> {ok, val()} | error().
get(_Db, _Key, _Opts) ->
    ?not_loaded.


-spec del(db(), ikey()) -> ok | error().
del(_Db, _Key) ->
    ?not_loaded.

-spec del(db(), ikey(), writeopts()) -> ok | {ok, snapshot()} | error().
del(_Db, _Key, _Opts) ->
    ?not_loaded.


-spec iter(db()) -> {ok, iterator()} | error().
iter(_Db) ->
    ?not_loaded.

-spec iter(db(), readopts()) -> {ok, iterator()} | error().
iter(_Db, _Opts) ->
    ?not_loaded.


-spec seek(iterator(), seek_dest()) -> {ok, {key(), val()}} | error().
seek(_Iter, _Key) ->
    ?not_loaded.

-spec next(iterator()) -> {ok, {key(), val()}} | error().
next(_Iter) ->
    ?not_loaded.

-spec prev(iterator()) -> {ok, {key(), val()}} | error().
prev(_Iter) ->
    ?not_loaded.


-spec batch(db()) -> {ok, write_batch()} | error().
batch(_Db) ->
    ?not_loaded.


-spec wb_put(write_batch(), ikey(), ival()) -> ok | error().
wb_put(Wb, Key, Val) when is_binary(Key) andalso is_binary(Val) ->
    wb_put0(Wb, Key, Val);
wb_put(Wb, Key, Val) ->
    wb_put0(Wb, ?i2b(Key), ?i2b(Val)).

-spec wb_del(write_batch(), ikey()) -> ok | error().
wb_del(Wb, Key) when is_binary(Key) ->
    wb_del0(Wb, Key);
wb_del(Wb, Key) ->
    wb_del0(Wb, ?i2b(Key)).

-spec wb_clear(write_batch()) -> ok | error().
wb_clear(_Wb) ->
    ?not_loaded.

-spec wb_write(write_batch()) -> ok | error().
wb_write(_Wb) ->
    ?not_loaded.

-spec wb_write(write_batch(), writeopts()) -> ok | {ok, snapshot()} | error().
wb_write(_Wb, _Opts) ->
    ?not_loaded.


-spec snapshot(db()) -> {ok, snapshot()} | error().
snapshot(_Db) ->
    ?not_loaded.


% Internal API

%% @private
-spec wb_put0(write_batch(), key(), val()) -> ok | error().
wb_put0(_Wb, _Key, _Val) ->
    ?not_loaded.


%% @private
-spec wb_del0(write_batch(), key()) -> ok | error().
wb_del0(_Wb, _Key) ->
    ?not_loaded.


%% @private
init() ->
    ModName = atom_to_list(?MODULE),
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", "priv"])) of
                true ->
                    filename:join(["..", "priv", ModName]);
                _ ->
                    filename:join(["priv", ModName])
            end;
        Dir ->
            filename:join(Dir, ModName)
    end,
    erlang:load_nif(SoName, 0).
