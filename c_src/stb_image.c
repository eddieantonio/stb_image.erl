#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <errno.h>

#include "erl_nif.h"
char * erl_errno_id(int error);

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

// Based on https://github.com/davisp/nif-examples/tree/master/apps/skeleton
// NIFs are awesome.

/* Adapted from:
 * https://github.com/davisp/nif-examples/blob/master/apps/termsend/c_src/termsend.c
 */

/*
 * Returns the existing atom or a new one if it does not exist.
 */
static ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

/*
 * Makes an error tuple, {error, Reason} where Reason is an atom.
 */
static ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load
static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

/*
 * Loads an image from the given binary filename.
 * Returns {ok, Binary} or {error, ProbableReason} where Reason is an atom.
 * Error reaons are NOT thread-safe, hence they are affixed with "Probable".
 */
static ERL_NIF_TERM
internal_load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;
    FILE *f = NULL;

    if (argc != 2) {
        enif_make_badarg(env);
    }

    ErlNifBinary name;
    ERL_NIF_TERM name_bin = argv[0];
    ERL_NIF_TERM depth = argv[1];

    /* Type checking and errors should have happened in Erlang.  */
    assert(enif_is_binary(env, name_bin));
    assert(enif_is_number(env, depth));

    int x, y, comp, req_comp;
    enif_get_int(env, depth, &req_comp);
    enif_inspect_binary(env, name_bin, &name);

    /* Use stbi_load_from_file() to catch stdio errors seperately. */
    f = fopen((const char *) name.data, "rb");
    if (f == NULL) {
        /* Note: Not thread-safe. */
        return mk_error(env, erl_errno_id(errno));
    }

    unsigned char *buffer = stbi_load_from_file(f, &x, &y, &comp, req_comp);

    /* Error checking! */
    if (buffer == NULL || comp == 0) {
        /* Note! Not thread-safe! */
        result = mk_error(env, stbi_failure_reason());
        goto finalize;
    }

    /* Copy the buffer over so that Erlang owns the binary. */
    size_t size = x * y * comp;
    unsigned char *erl_buffer = enif_make_new_binary(env, size, &result);
    memcpy(erl_buffer, buffer, size);
    stbi_image_free(buffer);

    result = enif_make_tuple2(env, enif_make_atom(env, "ok"), result);

finalize:
    if (f != NULL) {
        fclose(f);
    }
    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"internal_load", 2, internal_load}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(stb_image, nif_funcs, &load, NULL, NULL, NULL);
