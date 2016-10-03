#include "erl_nif.h"
#include <unistd.h>


static ERL_NIF_TERM stdout_isatty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if( isatty(STDOUT_FILENO) )
    {
        return enif_make_int(env, 0);
    }
    else
    {
        return enif_make_int(env, 1);
    }
}


static ErlNifFunc nif_funcs[] =
{
    {"isatty", 0, stdout_isatty}
};

ERL_NIF_INIT(applecore, nif_funcs, NULL, NULL, NULL, NULL)