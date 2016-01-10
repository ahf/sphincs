#ifndef crypto_stream_chacha12_H
#define crypto_stream_chacha12_H

#define crypto_stream_chacha12_@@@IMPLEMENTATION@@@_KEYBYTES 32
#define crypto_stream_chacha12_@@@IMPLEMENTATION@@@_NONCEBYTES 8

int crypto_stream_chacha12_@@@IMPLEMENTATION@@@(
  unsigned char *c,unsigned long long clen,
  const unsigned char *n,
  const unsigned char *k
);

int crypto_stream_chacha12_@@@IMPLEMENTATION@@@_xor(
  unsigned char *c,
  const unsigned char *m,unsigned long long mlen,
  const unsigned char *n,
  const unsigned char *k
);

#define crypto_stream_chacha12 crypto_stream_chacha12_@@@IMPLEMENTATION@@@
#define crypto_stream_chacha12_xor crypto_stream_chacha12_@@@IMPLEMENTATION@@@_xor
#define crypto_stream_chacha12_KEYBYTES crypto_stream_chacha12_@@@IMPLEMENTATION@@@_KEYBYTES
#define crypto_stream_chacha12_NONCEBYTES crypto_stream_chacha12_@@@IMPLEMENTATION@@@_NONCEBYTES

#endif
