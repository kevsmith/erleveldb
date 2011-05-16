%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.
%%
%% @author Paul J. Davis <paul.joseph.davis@gmail.com>
%% @copyright 2011 Paul J. Davis
%% @version 0.1.0
%% @reference <a href="http://leveldb.googlecode.com">LevelDB</a> 
%% @headerfile "erleveldb.hrl"
%% @doc
%% Erlang NIF driver to LevelDB.
%%
%% Ohai.
-module(erleveldb).
-on_load(init/0).


-include("erleveldb.hrl").

-define(i2b(B), iolist_to_binary(B)).
-define(location, [{module, ?MODULE}, {line, ?LINE}]).
-define(not_loaded, erlang:nif_error(not_loaded, [?location])).


-export([open_db/1, open_db/2, destroy_db/1]).
-export([put/3, put/4, get/2, get/3, del/2, del/3]).
-export([iter/1, iter/2, seek/2, next/1, prev/1]).
-export([batch/1, wb_put/3, wb_del/2, wb_clear/1, wb_write/1, wb_write/2]).
-export([snapshot/1]).


-export_type([db/0, iterator/0, write_batch/0, snapshot/0]).
-export_type([dbopts/0, readopts/0, writeopts/0, seek_dest/0]).


%% @doc Open an existing database.
%% This function can be used to open a database that was
%% created previously. <code>Name::iolist()</code> refers
%% to the path on disk to the database.
-spec open_db(iolist()) -> {ok, db()} | error().
open_db(_Name) ->
    ?not_loaded.

%% @doc Open an ErlLevelDB database.
%% This function can open or create a database. You'll
%% want to check out the <code>Opts::dbopts()</code> for
%% information on all the various knobs available.
%%
%% @see dbopts()
-spec open_db(iolist(), dbopts()) -> {ok, db()} | error().
open_db(_Name, _Opts) ->
    ?not_loaded.


%% @doc Destory an ErLevelDB database.
%% The actual database destruction will not occurr until the
%% db reference has been garbage collected.
-spec destroy_db(db()) -> ok.
destroy_db(_Db) ->
    ?not_loaded.


%% @doc Create a database snapshot.
%% Return a database snapshot. This allows for issuing
%% read requests against a specific version of the datbase.
%% Alternatively, the three write functions also have the
%% ability to return snapshots that represent a snapshot
%% just after the write is finished and before any other
%% write occurrs.
%%
%% To use a snapshot you just need to pass it to either of
%% the read methods in their readopts() options proplist.
%%
%% Multiple snapshots can exist for a given database at any
%% given time. It is important to note that a database will
%% not be closed until all snapshots are garbage collected.
-spec snapshot(db()) -> {ok, snapshot()} | error().
snapshot(_Db) ->
    ?not_loaded.


%% @doc Retrieve a value from the database.
%% Retrieve the value associated with the given key
%% or return <code>{error, not_found}</code> if the
%% key doesn't exist.
%%
%% @see get/3
-spec get(db(), iolist()) -> {ok, binary()} | error().
get(_Db, _Key) ->
    ?not_loaded.

%% @doc Retrieve a value from the database.
%% Retreive the value associated with the given key
%% or return <code>{error, not_found}</code> if the
%% key doesn't exist.
%%
%% @see readopts()
-spec get(db(), iolist(), readopts()) -> {ok, binary()} | error().
get(_Db, _Key, _Opts) ->
    ?not_loaded.


%% @doc Store a key/value pair in the database.
%%
%% @see put/4
-spec put(db(), iolist(), iolist()) -> ok | error().
put(_Db, _Key, _Value) ->
    ?not_loaded.


%% @doc Store a key/value pair in the database.
%% Write functions can also request that the database
%% call <code>fsync(2)</code> before returning using
%% the writeopts(). They can also request that a database
%% snapshot() is returned.
%%
%% @see writeopts()
%% @see snapshot/1
%% @see snapshot()
-spec put(db(), iolist(), iolist(), writeopts()) ->
        ok | {ok, binary()} | error().
put(_Db, _Key, _Value, _Opts) ->
    ?not_loaded.


%% @doc Remove a key from the database.
%% It is not an error if the key does not exist.
%% 
%% @see del/3
-spec del(db(), iolist()) -> ok | error().
del(_Db, _Key) ->
    ?not_loaded.

%% @doc Remove a key from the database.
%% It is not an error if the key does not exist.
%% Write functions can also request that the database
%% call <code>fsync(2)</code> before returning using
%% the writeopts(). They can also request a database
%% snapshot() is returned.
%%
%% @see writeopts()
%% @see snapshot/1
%% @see snapshot()
-spec del(db(), iolist(), writeopts()) -> ok | {ok, snapshot()} | error().
del(_Db, _Key, _Opts) ->
    ?not_loaded.


%% @doc Create a database iterator.
%% Returns a database iterator() that represents the
%% the current state of the database.
%%
%% @see iter/2
-spec iter(db()) -> {ok, iterator()} | error().
iter(_Db) ->
    ?not_loaded.

%% @doc Create a database iterator.
%% Returns a database iterator(). The iterator can
%% optionally refer to a specific version of the database
%% if a snapshot() is specified.
%%
%% It is important to note that a database will not be closed
%% until all iterators created from it are garbage collected.
%%
%% @see readopts()
-spec iter(db(), readopts()) -> {ok, iterator()} | error().
iter(_Db, _Opts) ->
    ?not_loaded.


%% @doc Seek an iterator to specific location.
%% This function returns they key/value pair at the
%% location requested or the first key/value after
%% the location request. If the specified location
%% is beyond the end of the database then
%% <code>{error, not_found}</code> is returned.
%% The atom <code>first</code> or <code>last</code>
%% can be used to seek to the first or last key
%% respectively.
-spec seek(iterator(), seek_dest()) -> {ok, {binary(), binary()}} | error().
seek(_Iter, _Key) ->
    ?not_loaded.

%% @doc Return the next key/value pair from the iterator.
%% If no more key/value pairs are available, then
%% <code>{error, not_found}</code> is returned.
-spec next(iterator()) -> {ok, {binary(), binary()}} | error().
next(_Iter) ->
    ?not_loaded.

%% @doc Return the previous key/value pair from the iterator.
%% If the iterator was already position before the first
%% key in the database, then <code>{error, not_found}</code>
%% is returned. Beware that reverse iteration may be
%% slower than forward iteration.
-spec prev(iterator()) -> {ok, {binary(), binary()}} | error().
prev(_Iter) ->
    ?not_loaded.


%% @doc Create a write_batch().
%% A write batch is used to make multiple updates to the
%% database in a single atomic action. It is important
%% to remember that the order of operations is important.
%% therefore, if you create a write batch that first put's
%% a key/value pair and then subsequently delete's that key,
%% the key will not exist after this write batch is executed.
%%
%% It is important to note that a database will not be closed
%% until all write batches created from it have been garbage
%% collected.
-spec batch(db()) -> {ok, write_batch()} | error().
batch(_Db) ->
    ?not_loaded.


%% @doc Queue a write in the given write batch.
%% Store the given key/value pair for a later write when this
%% write batch is committed. Remember that the order of
%% operations is important.
-spec wb_put(write_batch(), iolist(), iolist()) -> ok | error().
wb_put(Wb, Key, Val) when is_binary(Key) andalso is_binary(Val) ->
    wb_put0(Wb, Key, Val);
wb_put(Wb, Key, Val) ->
    wb_put0(Wb, ?i2b(Key), ?i2b(Val)).


%% @doc Queue the removal of a key in the given write batch.
%% Store they key for later removal when this write batch
%% is comitted. Remember that the order of operations is
%% important.
-spec wb_del(write_batch(), iolist()) -> ok | error().
wb_del(Wb, Key) when is_binary(Key) ->
    wb_del0(Wb, Key);
wb_del(Wb, Key) ->
    wb_del0(Wb, ?i2b(Key)).


%% @doc Clear a write batch.
%% Remove any wb_put or wb_del actions queued on this
%% write batch.
-spec wb_clear(write_batch()) -> ok | error().
wb_clear(_Wb) ->
    ?not_loaded.


%% @doc Commit a write batch to the database.
%% Apply the queued actions as an atomic unit against the
%% database from which it was created.
-spec wb_write(write_batch()) -> ok | error().
wb_write(_Wb) ->
    ?not_loaded.

%% @doc Commit a write batch to the database.
%% Apply the queued actions as an atomic unit against the
%% database from which it was created.
%%
%% @see writeopts()
-spec wb_write(write_batch(), writeopts()) -> ok | {ok, snapshot()} | error().
wb_write(_Wb, _Opts) ->
    ?not_loaded.


% Internal API

%% @private
-spec wb_put0(write_batch(), binary(), binary()) -> ok | error().
wb_put0(_Wb, _Key, _Val) ->
    ?not_loaded.


%% @private
-spec wb_del0(write_batch(), binary()) -> ok | error().
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
