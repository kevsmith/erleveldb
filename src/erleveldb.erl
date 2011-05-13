%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.

-module(erleveldb).

-include("erleveldb.hrl").

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).
-define(i2b(B), iolist_to_binary(B)).


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
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec open_db(dbname(), dbopts()) -> {ok, db()} | error().
open_db(_Name, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec put(db(), ikey(), ival()) -> ok | error().
put(_Db, _Key, _Value) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

-spec put(db(), ikey(), ival(), writeopts()) -> ok | {ok, val()} | error().
put(_Db, _Key, _Value, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        2 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec get(db(), ikey()) -> {ok, val()} | error().
get(_Db, _Key) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec get(db(), ikey(), readopts()) -> {ok, val()} | error().
get(_Db, _Key, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec del(db(), ikey()) -> ok | error().
del(_Db, _Key) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

-spec del(db(), ikey(), writeopts()) -> ok | {ok, snapshot()} | error().
del(_Db, _Key, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        2 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec iter(db()) -> iterator() | error().
iter(_Db) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            <<>>;
        _ ->
            {error, spoof}
    end.

-spec iter(db(), readopts()) -> iterator() | error().
iter(_Db, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            <<>>;
        _ ->
            {error, spoof}
    end.

-spec seek(iterator(), ikey()) -> iteration_result().
seek(_Iter, _Key) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            not_found;
        2 ->
            {<<"">>, <<"">>};
        _ ->
            {error, spoof}
    end.

-spec next(iterator()) -> iteration_result().
next(_Iter) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            not_found;
        2 ->
            {<<"">>, <<"">>};
        _ ->
            {error, spoof}
    end.

-spec prev(iterator()) -> iteration_result().
prev(_Iter) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            not_found;
        2 ->
            {<<"">>, <<"">>};
        _ ->
            {error, spoof}
    end.

-spec batch(db()) -> write_batch() | error().
batch(_Db) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            <<>>;
        _ ->
            {error, spoof}
    end.

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
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

-spec wb_write(write_batch()) -> ok | error().
wb_write(_Wb) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

-spec wb_write(write_batch(), writeopts()) -> ok | {ok, snapshot()} | error().
wb_write(_Wb, _Opts) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        2 ->
            {ok, <<>>};
        _ ->
            {error, spoof}
    end.

-spec snapshot(db()) -> snapshot() | error().
snapshot(_Db) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            <<>>;
        _ ->
            {error, spoof}
    end.

% Internal API

%% @private
-spec wb_put0(write_batch(), key(), val()) -> ok | error().
wb_put0(_Wb, _Key, _Val) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

%% @private
-spec wb_del0(write_batch(), key()) -> ok | error().
wb_del0(_Wb, _Key) ->
    ?NOT_LOADED,
    case random:uniform(1000) of
        1 ->
            ok;
        _ ->
            {error, spoof}
    end.

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

%% @private
not_loaded(Line) ->
    error_logger:error_msg("NIFs for ~p are NOT LOADED. Line: ~p~n", [?MODULE, Line]).
