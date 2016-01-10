#include <string.h>

#include <pthread.h>
#include <limits.h>

#include <sodium.h>

#include "erl_nif.h"

#include "../crypto_sign/sphincs256/ref/api.h"

#define STACK_SIZE (20 * 1024 * 1024)

static ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, char *error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

struct data {
    unsigned char *key;

    unsigned char *input;
    unsigned long long input_size;

    unsigned char *output;
    unsigned long long output_size;

    unsigned int status;
};

static ERL_NIF_TERM enif_sphincs_keypair(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    (void)argv;

    ErlNifBinary public;
    ErlNifBinary secret;

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    if (! enif_alloc_binary(CRYPTO_PUBLICKEYBYTES, &public)) {
        return make_error_tuple(env, "alloc_publickey_failed");
    }

    if (! enif_alloc_binary(CRYPTO_SECRETKEYBYTES, &secret)) {
        return make_error_tuple(env, "alloc_secretkey_failed");
    }

    if (crypto_sign_sphincs_keypair(public.data, secret.data) != 0) {
        return make_error_tuple(env, "keypair_failed");
    }

    return enif_make_tuple2(env, enif_make_binary(env, &public), enif_make_binary(env, &secret));
}

static void *sign_thread(void *x)
{
    struct data *data = x;
    data->status = crypto_sign_sphincs(data->output, &data->output_size, data->input, data->input_size, data->key);
    return NULL;
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

    unsigned char message_buf[message.size];
    memmove(message_buf, message.data, message.size);

    unsigned char signed_message_buf[CRYPTO_BYTES + message.size];

    struct data thread_data;
    thread_data.key = secret.data;

    thread_data.input = message.data;
    thread_data.input_size = message.size;

    thread_data.output = signed_message_buf;
    thread_data.output_size = 0;

    pthread_t signing_thread;
    pthread_attr_t attributes;

    pthread_attr_init(&attributes);
    if (pthread_attr_setstacksize(&attributes, STACK_SIZE) != 0) {
        return make_error_tuple(env, "pthread_attr_setstacksize_error");
    }

    if (pthread_create(&signing_thread, &attributes, sign_thread, &thread_data) != 0) {
        return make_error_tuple(env, "signing_thread_create_failed");
    }

    if (pthread_join(signing_thread, NULL) != 0) {
        return make_error_tuple(env, "signing_thread_join_failed");
    }

    if (thread_data.status != 0) {
        return make_error_tuple(env, "crypto_sign_failed");
    }

    if (! enif_alloc_binary(thread_data.output_size, &signed_message)) {
        return make_error_tuple(env, "alloc_signed_message_failed");
    }

    memmove(signed_message.data, signed_message_buf, thread_data.output_size);

    return enif_make_binary(env, &signed_message);
}

static void *verify_thread(void *x)
{
    struct data *data = x;
    data->status = crypto_sign_sphincs_open(data->output, &data->output_size, data->input, data->input_size, data->key);
    return NULL;
}

static ERL_NIF_TERM enif_sphincs_verify(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input;
    ErlNifBinary public;
    ErlNifBinary output;

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

    unsigned char output_buf[input.size + CRYPTO_BYTES];

    struct data thread_data;
    thread_data.key = public.data;

    thread_data.input = input.data;
    thread_data.input_size = input.size;

    thread_data.output = output_buf;
    thread_data.output_size = 0;

    pthread_t verification_thread;
    pthread_attr_t attributes;

    pthread_attr_init(&attributes);
    if (pthread_attr_setstacksize(&attributes, STACK_SIZE) != 0) {
        return make_error_tuple(env, "pthread_attr_setstacksize_error");
    }

    if (pthread_create(&verification_thread, &attributes, verify_thread, &thread_data) != 0) {
        return make_error_tuple(env, "verification_thread_create_failed");
    }

    if (pthread_join(verification_thread, NULL) != 0) {
        return make_error_tuple(env, "verification_thread_join_failed");
    }

    if (thread_data.status != 0) {
        return make_error_tuple(env, "failed_verification");
    }

    if (! enif_alloc_binary(thread_data.output_size, &output)) {
        return make_error_tuple(env, "alloc_output_failed");
    }

    memmove(output.data, thread_data.output, thread_data.output_size);

    return enif_make_binary(env, &output);
}

#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
#  define CPU_BOUND_NIF_FUNCTION(Name, Arity, Function) { Name, Arity, Function, ERL_NIF_DIRTY_JOB_CPU_BOUND }
#else
#  define CPU_BOUND_NIF_FUNCTION(Name, Arity, Function) { Name, Arity, Function }
#  warning "ERL_NIF_DIRTY_SCHEDULER_SUPPORT missing"
#endif

static ErlNifFunc nif_functions[] = {
    CPU_BOUND_NIF_FUNCTION("keypair", 0, enif_sphincs_keypair),
    CPU_BOUND_NIF_FUNCTION("sign", 2, enif_sphincs_sign),
    CPU_BOUND_NIF_FUNCTION("verify", 2, enif_sphincs_verify),
};

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)load_info;

    sodium_init();

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
