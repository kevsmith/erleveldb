#include "erl_nif.h"
#include "erl_nif_compat.h"

#include "leveldb/cache.h"
#include "leveldb/db.h"
#include "leveldb/options.h"
#include "leveldb/write_batch.h"

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

#define ENIF_IS(a, b) (enif_compare(a, b) == 0)

typedef struct {
    ErlNifResourceType*     db_res;
    ErlNifResourceType*     it_res;
    ErlNifResourceType*     wb_res;
} State;

typedef struct {
    ErlNifMutex*            lock;
    leveldb::DB*            db;
} DBRes;

typedef struct {
    ErlNifMutex*            lock;
    DBRes*                  dbres;
    leveldb::Iterator*      iter;
} IterRes;

typedef struct {
    ErlNifEnv*              env;
    ErlNifMutex*            lock;
    DBRes*                  dbres;
    leveldb::WriteBatch*    batch;
} WBRes;

void
free_dbres(ErlNifEnv* env, void* obj)
{
    DBRes* res = (DBRes*) obj;
    if(res->db != NULL) {
        delete res->db;
    }
}

void
free_itres(ErlNifEnv* env, void* obj)
{
    IterRes* res = (IterRes*) obj;
    enif_release_resource(res->dbres);
    if(res->iter != NULL) {
        delete res->iter;
    }
}

void
free_wbres(ErlNifEnv* env, void* obj)
{
    WBRes* res = (WBRes*) obj;
    enif_free_env(res->env);
    enif_release_resource(res->dbres);
    if(res->batch != NULL) {
        delete res->batch;
    }
}

static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);   
}

static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}

static int
set_db_opts(ErlNifEnv* env, ERL_NIF_TERM optlist, leveldb::Options& opts)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = optlist;
    const ERL_NIF_TERM* tuple;
    int arity;
    
    if(!enif_is_list(env, optlist)) {
        return 0;
    }
    
    while(enif_get_list_cell(env, tail, &head, &tail)) {
        if(ENIF_IS(head, make_atom(env, "create_if_missing"))) {
            if(opts.error_if_exists) return 0;
            opts.create_if_missing = true;
            continue;
        }
        
        if(ENIF_IS(head, make_atom(env, "error_if_exists"))) {
            if(opts.create_if_missing) return 0;
            opts.error_if_exists = true;
            continue;
        }
        
        if(ENIF_IS(head, make_atom(env, "paranoid_checks"))) {
            opts.paranoid_checks = true;
            continue;
        }
        
        if(!enif_get_tuple(env, head, &arity, &tuple)) {
            return 0;
        }
        
        if(arity != 2) return 0;
        
        if(ENIF_IS(tuple[0], make_atom(env, "write_buffer_size"))) {
            unsigned int sz;
            if(!enif_get_uint(env, tuple[1], (unsigned int*) &sz)) {
                return 0;
            }
            opts.write_buffer_size = (size_t) sz;
            continue;
        }
        
        if(ENIF_IS(tuple[0], make_atom(env, "max_open_files"))) {
            int num;
            if(!enif_get_int(env, tuple[1], &num)) {
                return 0;
            }
            opts.max_open_files = (size_t) num;
            continue;
        }
        
        if(ENIF_IS(tuple[0], make_atom(env, "cache_size"))) {
            unsigned int sz;
            if(!enif_get_uint(env, tuple[1], &sz)) {
                return 0;
            }
            opts.block_cache = leveldb::NewLRUCache((size_t) sz);
            continue;
        }
        
        if(ENIF_IS(tuple[0], make_atom(env, "block_size"))) {
            unsigned int sz;
            if(!enif_get_uint(env, tuple[1], &sz)) {
                return 0;
            }
            opts.block_size = (size_t) sz;
            continue;
        }
        
        if(ENIF_IS(tuple[0], make_atom(env, "block_restart_interval"))) {
            int bri;
            if(!enif_get_int(env, tuple[1], &bri)) {
                return 0;
            }
            opts.block_restart_interval = bri;
            continue;
        }
    }
    
    return 1;
}


BEGIN_C

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    State* st = (State*) enif_alloc(sizeof(State));
    const char* mod = "erleveldb";
    const char* db_name = "DBResource";
    const char* it_name = "IteratorResource";
    const char* wb_name = "WBResource";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* res;

    if(st == NULL) {
        return -1;
    }
    
    st->db_res = enif_open_resource_type(env, mod, db_name, free_dbres, 
                                        (ErlNifResourceFlags) flags, NULL);
    if(res == NULL) {
        enif_free(st);
        return -1;
    }
    
    st->it_res = enif_open_resource_type(env, mod, it_name, free_itres,
                                        (ErlNifResourceFlags) flags, NULL);
    if(res == NULL) {
        enif_free(st);
        return -1;
    }
    
    st->wb_res = enif_open_resource_type(env, mod, wb_name, free_wbres,
                                        (ErlNifResourceFlags) flags, NULL);
    if(res == NULL) {
        enif_free(st);
        return -1;
    }

    *priv = (void*) st;
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    *priv = *old_priv;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    if(priv != NULL) enif_free(priv);
}

static ERL_NIF_TERM
open_db(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);

    ErlNifBinary bin;
    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }
    std::string dbname((const char*) bin.data, bin.size);
    
    DBRes* res = (DBRes*) enif_alloc_resource(st->db_res, sizeof(DBRes));
    res->lock = NULL;
    res->db = NULL;

    leveldb::Options opts;
    if(!set_db_opts(env, argv[1], opts)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
 
    leveldb::Status status = leveldb::DB::Open(opts, dbname, &(res->db));
    if(!status.ok()) {
        enif_release_resource(res);
        return make_error(env, "db_init_failed");
    }
    
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    
    return make_ok(env, ret);
}

static ERL_NIF_TERM
dbput(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    DBRes* res;
    ErlNifBinary key;
    ErlNifBinary val;
    
    if(!enif_get_resource(env, argv[0], st->db_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_iolist_as_binary(env, argv[2], &val)) {
        return enif_make_badarg(env);
    }
    
    leveldb::Slice skey((const char*) key.data, key.size);
    leveldb::Slice sval((const char*) val.data, val.size);
    
    leveldb::Status s = res->db->Put(leveldb::WriteOptions(), skey, sval);
    if(s.ok()) {
        return make_atom(env, "ok");
    } else {
        return make_error(env, "unknown");
    }
}

static ERL_NIF_TERM
dbget(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    DBRes* res;
    ErlNifBinary key;
    
    if(!enif_get_resource(env, argv[0], st->db_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    
    leveldb::Slice skey((const char*) key.data, key.size);
    
    std::string val;
    leveldb::Status s = res->db->Get(leveldb::ReadOptions(), skey, &val);
    if(s.ok()) {
        ERL_NIF_TERM ret;
        unsigned char* buf = enif_make_new_binary(env, val.size(), &ret);
        memcpy(buf, val.data(), val.size());
        return make_ok(env, ret);
    } else if(s.IsNotFound()) {
        return make_error(env, "not_found");
    } else {
        return make_error(env, "unknown");
    }
}

static ERL_NIF_TERM
dbdel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    DBRes* res;
    ErlNifBinary key;
    
    if(!enif_get_resource(env, argv[0], st->db_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    
    leveldb::Slice skey((const char*) key.data, key.size);
    
    leveldb::Status s = res->db->Delete(leveldb::WriteOptions(), skey);
    if(s.ok()) {
        return make_atom(env, "ok");
    } else {
        return make_error(env, "unknown");
    }
}

static ERL_NIF_TERM
dbiter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    DBRes* dbres;
    
    if(!enif_get_resource(env, argv[0], st->db_res, (void**) &dbres)) {
        return enif_make_badarg(env);
    }
    
    IterRes* res = (IterRes*) enif_alloc_resource(st->it_res, sizeof(IterRes));
    res->lock = NULL;
    res->dbres = dbres;
    enif_keep_resource(dbres);

    res->iter = dbres->db->NewIterator(leveldb::ReadOptions());
    if(res->iter == NULL) {
        enif_release_resource(res);
        return make_error(env, "iterator_init_failed");
    }
    res->iter->SeekToFirst();

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    
    return make_ok(env, ret);
}

static ERL_NIF_TERM
itvalue(ErlNifEnv* env, IterRes* res)
{
    if(!res->iter->Valid()) {
        return make_atom(env, "not_found");
    }

    leveldb::Slice vslice = res->iter->value();
    
    ERL_NIF_TERM key, val;
    
    leveldb::Slice slice = res->iter->key();
    unsigned char* buf = enif_make_new_binary(env, slice.size(), &key);
    memcpy(buf, slice.data(), slice.size());
    
    slice = res->iter->value();
    buf = enif_make_new_binary(env, slice.size(), &val);
    memcpy(buf, slice.data(), slice.size());
    
    return enif_make_tuple2(env, key, val);    
}

static ERL_NIF_TERM
itseek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    IterRes* res;
    ErlNifBinary key;
    
    if(!enif_get_resource(env, argv[0], st->it_res, (void**) &res)) {
        return enif_make_badarg(env);
    }
    
    if(ENIF_IS(argv[1], make_atom(env, "first"))) {
        res->iter->SeekToFirst();
        if(res->iter->Valid()) {
            return itvalue(env, res);
        } else {
            return make_atom(env, "not_found");
        }
    } else if(ENIF_IS(argv[1], make_atom(env, "last"))) {
        res->iter->SeekToLast();
        if(res->iter->Valid()) {
            return itvalue(env, res);
        } else {
            return make_atom(env, "not_found");
        }
    } else if(enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        leveldb::Slice skey((const char*) key.data, key.size);
        res->iter->Seek(skey);
        if(res->iter->Valid()) {
            return itvalue(env, res);
        } else {
            return make_atom(env, "not_found");
        }
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM
itnext(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    IterRes* res;
    
    if(!enif_get_resource(env, argv[0], st->it_res, (void**) &res)) {
        return enif_make_badarg(env);
    }
    
    if(!res->iter->Valid()) {
        return make_atom(env, "not_found");
    }
    
    res->iter->Next();
    return itvalue(env, res);
}

static ERL_NIF_TERM
itprev(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    IterRes* res;
    
    if(!enif_get_resource(env, argv[0], st->it_res, (void**) &res)) {
        return enif_make_badarg(env);
    }
    
    if(!res->iter->Valid()) {
        return make_atom(env, "not_found");
    }
    
    res->iter->Prev();
    return itvalue(env, res);
}

static ERL_NIF_TERM
dbbatch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    DBRes* dbres;
    
    if(!enif_get_resource(env, argv[0], st->db_res, (void**) &dbres)) {
        return enif_make_badarg(env);
    }
    
    WBRes* res = (WBRes*) enif_alloc_resource(st->wb_res, sizeof(WBRes));
    res->env = enif_alloc_env();
    res->lock = NULL;
    res->dbres = dbres;
    enif_keep_resource(dbres);

    res->batch = new leveldb::WriteBatch();
    if(res->batch == NULL) {
        enif_release_resource(res);
        return make_error(env, "write_batch_init_failed");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    
    return make_ok(env, ret);
}

static ERL_NIF_TERM
wbput(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    WBRes* res;
    ErlNifBinary key;
    ErlNifBinary val;
    
    if(!enif_get_resource(env, argv[0], st->wb_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(res->batch == NULL) {
        return enif_make_badarg(env);
    }
    
    ERL_NIF_TERM argv1 = enif_make_copy(res->env, argv[1]);
    ERL_NIF_TERM argv2 = enif_make_copy(res->env, argv[2]);
    
    if(!enif_inspect_binary(env, argv1, &key)) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_binary(env, argv2, &val)) {
        return enif_make_badarg(env);
    }
    
    leveldb::Slice skey((const char*) key.data, key.size);
    leveldb::Slice sval((const char*) val.data, val.size);
    res->batch->Put(skey, sval);
    return make_atom(env, "ok");
}

static ERL_NIF_TERM
wbdel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    WBRes* res;
    ErlNifBinary key;
    
    if(!enif_get_resource(env, argv[0], st->wb_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(res->batch == NULL) {
        return enif_make_badarg(env);
    }
    
    ERL_NIF_TERM argv1 = enif_make_copy(res->env, argv[1]);
    
    if(!enif_inspect_binary(env, argv1, &key)) {
        return enif_make_badarg(env);
    }
    
    leveldb::Slice skey((const char*) key.data, key.size);
    res->batch->Delete(skey);
    return make_atom(env, "ok");
}

static ERL_NIF_TERM
wbclear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    WBRes* res;
    
    if(!enif_get_resource(env, argv[0], st->wb_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(res->batch == NULL) {
        return enif_make_badarg(env);
    }
    
    res->batch->Clear();
    return make_atom(env, "ok");
}

static ERL_NIF_TERM
wbwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    State* st = (State*) enif_priv_data(env);
    WBRes* res;
    
    if(!enif_get_resource(env, argv[0], st->wb_res, (void**) &res)) {
        return enif_make_badarg(env);
    } else if(res->batch == NULL) {
        return enif_make_badarg(env);
    }

    leveldb::WriteOptions opts;
    leveldb::Status s = res->dbres->db->Write(opts, res->batch);
    delete res->batch;
    res->batch = NULL;

    if(s.ok()) {
        return make_atom(env, "ok");
    } else {
        return make_error(env, "unknown");
    }
}

static ErlNifFunc funcs[] =
{
    {"open_db", 2, open_db},
    {"put", 3, dbput},
    {"get", 2, dbget},
    {"del", 2, dbdel},
    {"iter", 1, dbiter},
    {"seek", 2, itseek},
    {"next", 1, itnext},
    {"prev", 1, itprev},
    {"batch", 1, dbbatch},
    {"wb_put0", 3, wbput},
    {"wb_del0", 2, wbdel},
    {"wb_clear", 1, wbclear},
    {"wb_write", 1, wbwrite}
};

ERL_NIF_INIT(erleveldb, funcs, &load, &reload, &upgrade, &unload);

END_C
