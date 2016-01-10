/*
 * Copied from the eSTREAM api/ecrypt-sync.h,
 * and then edited to provide the crypto_stream/crypto_stream_xor interface.
 */

#include "crypto_stream_chacha12.h"

#include "e/ecrypt-sync.h"

int crypto_stream_chacha12_ref(
  unsigned char *c,unsigned long long clen,
  const unsigned char *n,
  const unsigned char *k
)
{
  ECRYPT_ctx ctx;
  unsigned long long i;
  ECRYPT_keysetup(&ctx,k,crypto_stream_chacha12_KEYBYTES * 8,crypto_stream_chacha12_NONCEBYTES * 8);
  ECRYPT_ivsetup(&ctx,n);

  for (i = 0;i < clen;++i)
      c[i] = 0;

  ECRYPT_encrypt_bytes(&ctx,c,c,clen);
  return 0;
}

int crypto_stream_chacha12_ref_xor(
  unsigned char *c,
  const unsigned char *m,unsigned long long mlen,
  const unsigned char *n,
  const unsigned char *k
)
{
  ECRYPT_ctx ctx;
  ECRYPT_keysetup(&ctx,k,crypto_stream_chacha12_KEYBYTES * 8,crypto_stream_chacha12_NONCEBYTES * 8);
  ECRYPT_ivsetup(&ctx,n);
  ECRYPT_encrypt_bytes(&ctx,m,c,mlen);
  return 0;
}
