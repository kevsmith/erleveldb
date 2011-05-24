%% This file is part of ErLevelDB released under the MIT license. 
%% See the LICENSE file for more information.

%% @type db().
%% An opaque handle to the database.
-type db() :: term().


%% @type iterator().
%% An opaque handle to a database iterator.
-type iterator() :: term().


%% @type write_batch().
%% An opaque handle to a write batch.
-type write_batch() :: term().


%% @type snapshot().
%% An opaque handle to a database snapshot.
-type snapshot() :: term().


%% @type dbopts(). 
%% Options for creating databases.
%% <dl>
%%   <dt>create_if_missing</dt>
%%   <dd>
%%     Create the database if it doesn't exist.
%%   </dd>
%%   <dt>error_if_exists</dt>
%%   <dd>
%%     The database must not exist before creating it.
%%   </dd>
%%   <dt>paranoid_checks</dt>
%%   <dd>
%%     Instruct the database code to check extensively for
%%     corruption the the database files.
%%   </dd>
%%   <dt>{write_buffer_size, pos_integer()}</dt>
%%   <dd>
%%     The amount of RAM in bytes to use to buffer writes
%%     before they are sorted and written to disk. Up to 2x
%%     the <code>write_buffer_size</code> may be stored in
%%     RAM at one time. The default is 4MiB.
%%   </dd>
%%   <dt>{max_open_files, pos_integer()}</dt>
%%   <dd>
%%     The maximum number of files to keep open for accessing
%%     the database. The default is 100 open files.
%%   </dd>
%%   <dt>{cache_size, pos_integer()}</dt>
%%   <dd>
%%     The amount of RAM in bytes to use as a cache for
%%     frequently read data blocks. The default is 8MiB.
%%   </dd>
%%   <dt>{block_size, pos_integer()}</dt>
%%   <dd>
%%     The size of a data block when writing to disk. The
%%     default is 4KiB.
%%   </dd>
%%   <dt>{block_restart_interval, pos_integer()}</dt>
%%   <dd>
%%     The number of keys between restart points when delta
%%     encoding key prefixes. The default is 16. This option
%%     is usually left to the default.
%%   </dd>
%% </dl>
-type dbopts() :: [
      create_if_missing
    | error_if_exists
    | paranoid_checks
    | {write_buffer_size, pos_integer()}
    | {max_open_files, pos_integer()}
    | {cache_size, pos_integer()}
    | {block_size, pos_integer()}
    | {block_restart_interval, pos_integer()}
].


%% @type readopts().
%% Options when issuing read requests.
%% <dl>
%%   <dt>verify_checksums</dt>
%%   <dd>
%%     Verify the checksums for data read during this
%%     request.
%%   </dd>
%%   <dt>skip_cache</dt>
%%   <dd>
%%     Do not store data read into the block cache. This
%%     is mostly useful for bulk reads when you don't expect
%%     to reread the data quickly.
%%   </dd>
%%   <dt>{snapshot, snapshot()}</dt>
%%   <dd>
%%     A database snapshot to read from. This will only return
%%     results that existed at a given state of the database.
%%   </dd>
%% </dl>
-type readopts() :: [
      verify_checksums
    | skip_cache
    | {snapshot, snapshot()}
].


%% @type writeopts().
%% Options when issuing write requests.
%% <dl>
%%   <dt>sync</dt>
%%   <dd>
%%     Call <code>fsync(2)</code> after the write
%%     operation to flush the operating system buffers
%%     to disk.
%%   </dd>
%%   <dt>snapshot</dt>
%%   <dd>
%%     Returns <code>{ok, snapshot()}</code> where the
%%     snapshot refers to a logical point in time just
%%     after this write complete but before any other
%%     modification to the database.
%%   </dd>
%% </dl>
-type writeopts() :: [sync | snapshot].


%% @type seek_dest().
%% Iterators can seek to a provided key to start iterating
%% from. The two atoms <code>first</code> and
%% <code>last</code> can be used to seek to the very first or
%% last key in the database.
-type seek_dest() :: iolist() | first | last.


%% @type error().
%% The format for all errors reported by ErLevelDB.
-type error() :: {error, any()}.
