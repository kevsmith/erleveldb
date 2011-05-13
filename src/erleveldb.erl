%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.
-module(erleveldb).
-on_load(init/0).

-include("erleveldb.hrl").

-define(i2b(B), iolist_to_binary(B)).


-define(spoof_rval1(A), case random:uniform(100) + 3 of
    1 -> A; _ -> {error, {not_loaded, ?MODULE, ?LINE}}
end).

-define(spoof_rval2(A, B), case random:uniform(100) + 3 of
    1 -> A; 2 -> B; _ -> {error, {not_loaded, ?MODULE, ?LINE}}
end).


-define(bin, ?spoof_rval1({ok, <<>>})).
-define(kv, ?spoof_rval1({ok, {<<>>, <<>>}})).
-define(ok, ?spoof_rval1(ok)).
-define(ok_bin, ?spoof_rval2(ok, {ok, <<>>})).


-export([open_db/1, open_db/2]).
-export([put/3, put/4, get/2, get/3, del/2, del/3]).
-export([iter/1, iter/2, seek/2, next/1, prev/1]).
-export([batch/1, wb_put/3, wb_del/2, wb_clear/1, wb_write/1, wb_write/2]).
-export([snapshot/1]).


-export_type([db/0, iterator/0, write_batch/0, snapshot/0]).
-export_type([dbname/0, dbopts/0, readopts/0, writeopts/0]).
-export_type([ikey/0, ival/0, key/0, val/0, seek_dest/0]).


-spec open_db(dbname()) -> {ok, db()} | error().
open_db(_Name) -> ?bin.

-spec open_db(dbname(), dbopts()) -> {ok, db()} | error().
open_db(_Name, _Opts) -> ?bin.


-spec put(db(), ikey(), ival()) -> ok | error().
put(_Db, _Key, _Value) -> ?ok.

-spec put(db(), ikey(), ival(), writeopts()) -> ok | {ok, val()} | error().
put(_Db, _Key, _Value, _Opts) -> ?ok_bin.


-spec get(db(), ikey()) -> {ok, val()} | error().
get(_Db, _Key) -> ?bin.

-spec get(db(), ikey(), readopts()) -> {ok, val()} | error().
get(_Db, _Key, _Opts) -> ?bin.


-spec del(db(), ikey()) -> ok | error().
del(_Db, _Key) -> ?ok.

-spec del(db(), ikey(), writeopts()) -> ok | {ok, snapshot()} | error().
del(_Db, _Key, _Opts) -> ?ok_bin.


-spec iter(db()) -> {ok, iterator()} | error().
iter(_Db) -> ?bin.

-spec iter(db(), readopts()) -> {ok, iterator()} | error().
iter(_Db, _Opts) -> ?bin.


-spec seek(iterator(), seek_dest()) -> {ok, {key(), val()}} | error().
seek(_Iter, _Key) -> ?kv.

-spec next(iterator()) -> {ok, {key(), val()}} | error().
next(_Iter) -> ?kv.

-spec prev(iterator()) -> {ok, {key(), val()}} | error().
prev(_Iter) -> ?kv.


-spec batch(db()) -> {ok, write_batch()} | error().
batch(_Db) -> ?bin.


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
wb_clear(_Wb) -> ?ok.

-spec wb_write(write_batch()) -> ok | error().
wb_write(_Wb) -> ?ok.

-spec wb_write(write_batch(), writeopts()) -> ok | {ok, snapshot()} | error().
wb_write(_Wb, _Opts) -> ?ok_bin.


-spec snapshot(db()) -> {ok, snapshot()} | error().
snapshot(_Db) -> ?bin.


% Internal API

%% @private
-spec wb_put0(write_batch(), key(), val()) -> ok | error().
wb_put0(_Wb, _Key, _Val) -> ?ok.


%% @private
-spec wb_del0(write_batch(), key()) -> ok | error().
wb_del0(_Wb, _Key) -> ?ok.


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
