/* vim: set sw=4 sts=4 et foldmethod=syntax : */

#include <sodium.h>

void randombytes(unsigned char *buf,unsigned long long n)
{
    randombytes_buf(buf, n);
}
