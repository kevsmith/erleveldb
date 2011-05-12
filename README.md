# ErLevelDB

This is a NIF to connect Erlang the [LevelDB][leveldb] library released
by Google. Its pretty basic so far and I haven't done much testing with
it beyond asserting that the basic API works.

# API

This API description uses a faux spec language to describe the Erlang
types associated with each function. I'm not entirely certain how they'd
interact with NIF functions or how to generate API docs from Erlang
modules just yet so I'm running with this for now. When I get time I'll
clean things up.

## Opening a database

    db() -> term()
    dbname() -> iolist()    
    dbopts() -> proplist()

    erleveldb:open_db(dbname()) -> {ok, db()}
    erleveldb:open_db(dbname(), dbopts()) -> {ok, db()}

### Valid values for the `dbopts()` proplist

* `create_if_missing` Create a new database disk structure if the
  database does not exist.
* `error_if_exists` Abort opening the database if it already exists.
* `paranoid_checks` Instruct the database code to check extensively for
  corruption the the database files.
* `{write_buffer_size, pos_integer()}` The amount of RAM in bytes to use
  to buffer writes before they are sorted and written to disk. Up to 2x the
  `write_buffer_size` may be stored in RAM at one time. The default is 4MiB.
* `{max_open_files, pos_integer()}` The maximum number of files to keep
  open for accessing the database. The default is 100 open files.
* `{cache_size, pos_integer()}` The amount of RAM in bytes to use as a
  cache for frequently read data blocks. The default is 8MiB.
* `{block_size, pos_integer()}` The size of a data block when writing to
  disk. The default is 4KiB.
* `{block_restart_interval, post_integer()}` The number of keys between
  restart points when delta encoding key prefixes. The default is 16. This
  option is usually left to the default.


## Storing and Retrieving Data

    snapshot() -> term()
    key() -> iolist()
    val() -> iolist()
    result() -> binary()
    readopts() -> proplist()
    writeopts() -> proplist()
    
    erleveldb:get(db(), key()) -> {ok, result()}
    erleveldb:get(db(), key(), readopts()) -> {ok, result()}
    
    erleveldb:put(db(), key(), val()) -> ok
    erleveldb:put(db(), key(), val(), writeopts()) -> ok | {ok, snapshot()}

    erleveldb:del(db(), key()) -> ok
    erleveldb:del(db(), key(), writeopts()) -> ok | {ok, snapshot()}

### Valid values for the `readopts()` proplist

* `verify_checksums` Verify the checksums for data read during this
  request.
* `skip_cache` Do not store data read into the block cache. This is
  mostly useful for bulk reads when you don't expect to reread the
  data quickly.
* `{snapshot, Snapshot}` A database snapshot to read from. This will
  only return results that existed at a given state of the database.

### Valid values for the `writeopts()` proplist

* `sync` Call `fsync(2)` after the write operation to flush the
  operating system buffers to disk.
* `snapshot` Returns `{ok, snapshot()}` where the snapshot refers to
  a logical point in time just after this write complete but before
  any other modification to the database.


## Database Iteration

    iterator() -> term()
    dest() -> key() | first | last
    key() -> binary()
    val() -> binary()
    
    erleveldb:iter(db()) -> iterator()
    erleveldb:iter(db(), readopts()) -> iteratar()
    
    erleveldb:seek(db(), dest()) -> not_found | {key(), val()}
    
    erleveldb:next(db()) -> not_found | {key(), val()}
    erleveldb:prev(db()) -> not_found | {key(), val()}
    
The values for readopts() are the same as above. This API is a bit wonky
in so much as the `seek/2` returns the first key/value pair in the iterator.
This may change in the future.

It is important to note that a database will not be closed until all
iterators created from it are garbage collected.

## Batched updates

    write_batch() -> term()
    key() -> iolist()
    val() -> iolist()
    
    erleveldb:batch(db()) -> write_batch()
    
    erleveldb:wb_put(write_batch(), key(), val()) -> ok
    erleveldb:wb_del(write_batch(), key()) -> ok
    
    erleveldb:wb_clear(write_batch()) -> ok
    
    erleveldb:wb_write(write_batch()) -> ok
    erleveldb:wb_write(write_batch(), writeopts()) -> ok | {ok | snapshot()}

Batched updates can be used to apply a series of put and delete operations
against a database as an atomic unit. The order of put and delete operations
is executed in the order specified. Thus, if you call `wb_put/3` with a key
and then subsequently call `wb_del/2` with the same key, that particular key
will not exist after the write batch is applied. Calling `wb_clear/1` will
empty the queued set of operations from this write batch.

The writeopts() are the same as described above.

It is important to note that a database will not be closed until all
write batches created from it have been garbage collected.

## Snapshots

    erleveldb:snapshot(db()) -> snapshot()
    
Snapshots are used to issue reads against a specific version of the
database. They can be returned from the whatever the current version of
the database happens to be with `snapshot/1` or they can be returned
from any of the three calls that make updates (`put`, `del`, `wb_write`).

To use a snapshot you just need to pass it to either of the read methods
in their `readopts()` options proplist.

Multiple snapshots can exist for a given database at any given time. It is
important to note that a database will not be closed until all snapshots
are garbage collected.

[leveldb]: http://leveldb.googlecode.com "The LevelDB Project"