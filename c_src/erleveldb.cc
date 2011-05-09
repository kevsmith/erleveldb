#include "erl_nif.h"
#include "erl_nif_compat.h"

#include "leveldb/db.h"

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

typedef struct {
    ErlNifResourceType*     db_res;
} State;

typedef struct {
    ErlNifMutex*            lock;
    leveldb::DB*            db;
} DBRes;

void
free_dbres(ErlNifEnv* env, void* obj)
{
    DBRes* res = (DBRes*) obj;
    if(res->db != NULL) {
        delete res->db;
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

BEGIN_C

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    State* st = (State*) enif_alloc(sizeof(State));
    const char* mod = "erleveldb";
    const char* name = "DBResource";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* res;

    if(st == NULL) {
        return -1;
    }
    
    res = enif_open_resource_type(env, mod, name, free_dbres, 
                                    (ErlNifResourceFlags) flags, NULL);
    if(res == NULL) {
        enif_free(st);
        return -1;
    }
    
    st->db_res = res;
    
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
    const char* dbname = (const char*) bin.data;

    DBRes* res = (DBRes*) enif_alloc_resource(st->db_res, sizeof(DBRes));
    res->lock = enif_mutex_create(NULL);
    res->db = NULL;

    leveldb::Options opts;
    opts.create_if_missing = true;
 
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
    if(!s.ok()) {
        return make_error(env, "put_failed");
    }
    
    return make_atom(env, "ok");
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


static ErlNifFunc funcs[] =
{
    {"open_db", 1, open_db},
    {"put", 3, dbput},
    {"get", 2, dbget}
};

ERL_NIF_INIT(erleveldb, funcs, &load, &reload, &upgrade, &unload);

END_C
