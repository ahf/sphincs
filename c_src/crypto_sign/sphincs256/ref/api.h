#include "params.h"

#define CRYPTO_SECRETKEYBYTES (SEED_BYTES + CRYPTO_PUBLICKEYBYTES-HASH_BYTES + SK_RAND_SEED_BYTES)
#define CRYPTO_PUBLICKEYBYTES ((N_MASKS+1)*HASH_BYTES)
#define CRYPTO_BYTES (MESSAGE_HASH_SEED_BYTES + (TOTALTREE_HEIGHT+7)/8 + HORST_SIGBYTES + (TOTALTREE_HEIGHT/SUBTREE_HEIGHT)*WOTS_SIGBYTES + TOTALTREE_HEIGHT*HASH_BYTES)
#define CRYPTO_DETERMINISTIC 1

int crypto_sign_sphincs_keypair(unsigned char *pk, unsigned char *sk);
int crypto_sign_sphincs(unsigned char *sm, unsigned long long *smlen, const unsigned char *m,unsigned long long mlen, const unsigned char *sk);
int crypto_sign_sphincs_open(unsigned char *m,unsigned long long *mlen, const unsigned char *sm,unsigned long long smlen, const unsigned char *pk);
