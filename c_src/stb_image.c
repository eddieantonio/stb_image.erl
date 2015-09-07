#include <assert.h>
#include <string.h>

#include "erl_nif.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

static ERL_NIF_TERM
internal_load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        enif_make_badarg(env);
    }

    ERL_NIF_TERM name_bin = argv[0];
    ERL_NIF_TERM depth = argv[1];
    ErlNifBinary name;

    assert(enif_is_binary(env, name_bin));
    assert(enif_is_number(env, depth));

    int x, y, comp, req_comp;
    enif_get_int(env, depth, &req_comp);
    enif_inspect_binary(env, name_bin, &name);

    unsigned char *buffer = stbi_load((char *) name.data, &x, &y, &comp, req_comp);

    /* Error checking! */
    if (buffer == NULL || comp == 0) {
        return enif_make_tuple2(env,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "stb_image"));
    }

    /* Copy the buffer over. */
    size_t size = x * y * comp;
    ERL_NIF_TERM result;

    /* Bundle up into a binary. */
    unsigned char *erl_buffer = enif_make_new_binary(env, size, &result);
    memcpy(erl_buffer, buffer, size);
    stbi_image_free(buffer);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ErlNifFunc nif_funcs[] = {
    {"internal_load", 2, internal_load}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(stb_image, nif_funcs, &load, NULL, NULL, NULL);
