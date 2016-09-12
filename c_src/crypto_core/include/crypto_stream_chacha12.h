#ifndef crypto_stream_chacha12_H
#define crypto_stream_chacha12_H

#define crypto_stream_chacha12_ref_KEYBYTES 32
#define crypto_stream_chacha12_ref_NONCEBYTES 8

int crypto_stream_chacha12_ref(
  unsigned char *c,unsigned long long clen,
  const unsigned char *n,
  const unsigned char *k
);

int crypto_stream_chacha12_ref_xor(
  unsigned char *c,
  const unsigned char *m,unsigned long long mlen,
  const unsigned char *n,
  const unsigned char *k
);

#define crypto_stream_chacha12 crypto_stream_chacha12_ref
#define crypto_stream_chacha12_xor crypto_stream_chacha12_ref_xor
#define crypto_stream_chacha12_KEYBYTES crypto_stream_chacha12_ref_KEYBYTES
#define crypto_stream_chacha12_NONCEBYTES crypto_stream_chacha12_ref_NONCEBYTES

#endif
