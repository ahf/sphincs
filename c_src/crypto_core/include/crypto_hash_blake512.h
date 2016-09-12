#ifndef crypto_hash_blake512_H
#define crypto_hash_blake512_H

#define crypto_hash_blake512_ref_BYTES 64

#ifdef __cplusplus
extern "C" {
#endif
extern int crypto_hash_blake512_ref(unsigned char *,const unsigned char *,unsigned long long);
#ifdef __cplusplus
}
#endif

#define crypto_hash_blake512 crypto_hash_blake512_ref
#define crypto_hash_blake512_BYTES crypto_hash_blake512_ref_BYTES

#endif
