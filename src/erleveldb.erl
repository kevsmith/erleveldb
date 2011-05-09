-module(erleveldb).
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-export([open_db/1]).
-export([put/3, get/2, del/2]).

open_db(_Name) ->
    ?NOT_LOADED.

put(_Db, _Key, _Value) ->
    ?NOT_LOADED.

get(_Db, _Key) ->
    ?NOT_LOADED.

del(_Db, _Key) ->
    ?NOT_LOADED.

init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).