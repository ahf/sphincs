/* vim: set sw=4 sts=4 et foldmethod=syntax : */

#include <sodium.h>

static int initialized;

void randombytes(unsigned char *buf,unsigned long long n)
{
    if (! initialized) {
        sodium_init();
        initialized = 1;
    }

    randombytes_buf(buf, n);
}
