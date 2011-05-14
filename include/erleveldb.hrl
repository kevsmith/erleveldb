%% Opaque resource types
-type db() :: term().
-type iterator() :: term().
-type write_batch() :: term().
-type snapshot() :: term().

%% Database names and various options
-type dbopts() :: [
      create_if_missing
    | error_if_exists
    | paranoid_checks
    | {write_buffer_size, pos_integer()}
    | {cache_size, pos_integer()}
    | {block_size, pos_integer()}
    | {block_restart_interval, pos_integer()}
].

-type readopts() :: [
      verify_checksums
    | skip_cache
    | {snapshot, snapshot()}
].

-type writeopts() :: [sync | snapshot].

%% Misc
-type seek_dest() :: iolist() | first | last.
-type error() :: {error, any()}.
