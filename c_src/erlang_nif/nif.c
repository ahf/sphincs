#include <string.h>

#include "erl_nif.h"

#include "../crypto_sign/sphincs256/ref/api.h"

static ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, char *error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

static ERL_NIF_TERM enif_sphincs_keypair(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    (void)argv;

    ErlNifBinary public;
    ErlNifBinary secret;
    ErlNifBinary random;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[0], &random)) {
        return enif_make_badarg(env);
    }

    if (random.size != CRYPTO_SECRETKEYBYTES) {
        return make_error_tuple(env, "invalid_random");
    }

    if (! enif_alloc_binary(CRYPTO_PUBLICKEYBYTES, &public)) {
        return make_error_tuple(env, "alloc_publickey_failed");
    }

    if (! enif_alloc_binary(CRYPTO_SECRETKEYBYTES, &secret)) {
        return make_error_tuple(env, "alloc_secretkey_failed");
    }

    memmove(secret.data, random.data, secret.size);

    if (crypto_sign_sphincs_keypair(public.data, secret.data) != 0) {
        return make_error_tuple(env, "keypair_failed");
    }

    return enif_make_tuple2(env, enif_make_binary(env, &public), enif_make_binary(env, &secret));
}

static ERL_NIF_TERM enif_sphincs_sign(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary message;
    ErlNifBinary secret;
    ErlNifBinary signed_message;

    if (! enif_inspect_iolist_as_binary(env, argv[0], &message)) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[1], &secret)) {
        return enif_make_badarg(env);
    }

    if (secret.size != CRYPTO_SECRETKEYBYTES) {
        return make_error_tuple(env, "invalid_secret_key");
    }

    unsigned char output[CRYPTO_BYTES + message.size];
    unsigned long long output_size = 0;

    if (crypto_sign_sphincs(output, &output_size, message.data, message.size, secret.data) != 0) {
        return make_error_tuple(env, "crypto_sign_failed");
    }

    if (! enif_alloc_binary(output_size, &signed_message)) {
        return make_error_tuple(env, "alloc_signed_message_failed");
    }

    memmove(signed_message.data, output, output_size);

    return enif_make_binary(env, &signed_message);
}

static ERL_NIF_TERM enif_sphincs_verify(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input;
    ErlNifBinary public;
    ErlNifBinary message;

    if (! enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[1], &public)) {
        return enif_make_badarg(env);
    }

    if (public.size != CRYPTO_PUBLICKEYBYTES) {
        return make_error_tuple(env, "invalid_public_key");
    }

    if (input.size < CRYPTO_BYTES) {
        return make_error_tuple(env, "invalid_signature");
    }

    unsigned char output[input.size + CRYPTO_BYTES];
    unsigned long long output_size = 0;

    if (crypto_sign_sphincs_open(output, &output_size, input.data, input.size, public.data) != 0) {
        return make_error_tuple(env, "failed_verification");
    }

    if (! enif_alloc_binary(output_size, &message)) {
        return make_error_tuple(env, "alloc_message_failed");
    }

    memmove(message.data, output, output_size);

    return enif_make_binary(env, &message);
}

#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
#  define CPU_BOUND_NIF_FUNCTION(Name, Arity, Function) { Name, Arity, Function, ERL_NIF_DIRTY_JOB_CPU_BOUND }
#else
#  define CPU_BOUND_NIF_FUNCTION(Name, Arity, Function) { Name, Arity, Function }
#  warning "ERL_NIF_DIRTY_SCHEDULER_SUPPORT missing"
#endif

static ErlNifFunc nif_functions[] = {
    CPU_BOUND_NIF_FUNCTION("keypair", 1, enif_sphincs_keypair),
    CPU_BOUND_NIF_FUNCTION("sign", 2, enif_sphincs_sign),
    CPU_BOUND_NIF_FUNCTION("verify", 2, enif_sphincs_verify),
};

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)load_info;

    return 0;
}

static int on_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)old_priv_data;
    (void)load_info;

    return 0;
}

ERL_NIF_INIT(sphincs_nif, nif_functions, on_load, /* reload */ NULL, on_upgrade, /* unload */ NULL);
