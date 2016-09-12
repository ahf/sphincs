#ifndef crypto_hash_blake256_H
#define crypto_hash_blake256_H

#define crypto_hash_blake256_ref_BYTES 32

#ifdef __cplusplus
extern "C" {
#endif
extern int crypto_hash_blake256_ref(unsigned char *,const unsigned char *,unsigned long long);
#ifdef __cplusplus
}
#endif

#define crypto_hash_blake256 crypto_hash_blake256_ref
#define crypto_hash_blake256_BYTES crypto_hash_blake256_ref_BYTES

#endif
