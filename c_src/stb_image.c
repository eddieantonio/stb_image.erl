#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
char * erl_errno_id(int error);

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

// Based on https://github.com/davisp/nif-examples/tree/master/apps/skeleton
// NIFs are awesome.

/*
 * The following helpers adapted from:
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

static ERL_NIF_TERM
mk_prefixed_pair(ErlNifEnv *env, const char *atom, ERL_NIF_TERM term)
{
    return enif_make_tuple2(env, mk_atom(env, atom), term);
}

/*
 * Makes an error tuple, {error, Reason}.
 */
static ERL_NIF_TERM
mk_error_term(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return mk_prefixed_pair(env, "error", term);
}

/*
 * Makes an error tuple, {error, Reason} where Reason is an atom.
 */
static ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return mk_error_term(env, mk_atom(env, mesg));
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
    ERL_NIF_TERM binary, result;
    FILE *f = NULL;
    unsigned char *buffer = NULL;
    int x, y, comp, req_comp;
    x = y = comp = -1;

    if (argc != 2) {
        enif_make_badarg(env);
    }

    ErlNifBinary name;
    ERL_NIF_TERM name_bin = argv[0];
    ERL_NIF_TERM depth = argv[1];

    /* Type checking and errors should have happened in Erlang.  */
    assert(enif_is_binary(env, name_bin));
    assert(enif_is_number(env, depth));

    enif_get_int(env, depth, &req_comp);
    enif_inspect_binary(env, name_bin, &name);

    /* Use stbi_load_from_file() to catch stdio errors seperately. */
    f = fopen((const char *) name.data, "rb");
    if (f == NULL) {
        /* Note: Not thread-safe. */
        return mk_error(env, erl_errno_id(errno));
    }

    buffer = stbi_load_from_file(f, &x, &y, &comp, req_comp);

    /* Error checking! */
    if (buffer == NULL || comp <= 0) {
        /* Note! Not thread-safe! */
        result = mk_error_term(
            env,
            mk_prefixed_pair(
                env,
                "stb_image",
                enif_make_string(env, stbi_failure_reason(), ERL_NIF_LATIN1)
            )
        );
        goto finalize;
    }

    assert(x > 0 && y > 0);

    /* Copy the buffer over so that Erlang owns the binary. */
    size_t size = x * y * comp;
    unsigned char *erl_buffer = enif_make_new_binary(env, size, &binary);
    if (erl_buffer == NULL) {
        result = mk_error(env, "nif");
        goto finalize;
    }

    memcpy(erl_buffer, buffer, size);

    /* Return: {ok, {X, Y, NComponents, Binary}}. */
    result = mk_prefixed_pair(
        env,
        "ok",
        enif_make_tuple4(
            env,
            enif_make_int(env, x),
            enif_make_int(env, y),
            enif_make_int(env, comp),
            binary
        )
    );

finalize:
    if (f != NULL) {
        fclose(f);
    }
    if (buffer != NULL) {
        stbi_image_free(buffer);
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

ERL_NIF_INIT(stb_image, nif_funcs, NULL, NULL, NULL, NULL);
