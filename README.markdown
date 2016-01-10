SPHINCS-256  NIF for Erlang
===========================

[![Build Status](https://travis-ci.org/ahf/sphincs.svg?branch=develop)](https://travis-ci.org/ahf/sphincs)

SPHINCS-256 is a high-security post-quantum stateless hash-based signature
scheme. This repository contains the SPHINCS-256 implementation found in the
SUPERCOP performance suite together with an Erlang NIF's for the SPHINCS API.

SPHINCS-256 uses 41 KB signatures, 1 KB public keys, and 1 KB private keys.

For more information about SPHINCS see:

- http://sphincs.cr.yp.to
- http://bench.cr.yp.to/supercop.html

## Usage

1. Create a keypair.

        1> #{ secret := Secret, public := Public } = sphincs:keypair().
        #{public => <<188,75,148,147,203,240,152,149,40,103,197,233,4,199,
            245,227,31,251,245,128,2,168,21,194,235,35,151,252,...>>,
          secret => <<190,137,81,29,80,110,124,82,179,203,5,180,19,120,3,111,
            79,205,116,240,52,112,78,227,78,142,140,...>>}

2. Sign a document.

        2> SignedDocument = sphincs:sign(<<"Hello world!">>, Secret).
        <<169,24,140,16,104,204,246,11,124,204,196,91,67,108,125,
          104,108,77,153,127,231,174,138,213,44,62,250,130,253,...>>

3. Verify a signed document.

        3> sphincs:verify(SignedDocument, Public).
        {ok,<<"Hello world!">>}

## Current Issues

- SPHINCS-256 uses "large", stack allocated, buffers for working with the 41 KB
  signatures, which causes troubles for Erlang's BEAM VM.

  On my Mac, it seems that the Erlang VM uses a worker thread to start the dirty
  scheduler on, which then executes the NIF itself. This causes troubles because
  the stack size for a thread is smaller than the stack size of an ordinary
  program. We have worked around this with a very dirty hack where we create a
  pthread, with a larger stack than the one the Erlang VM uses for the dirty
  scheduler, which is immediately joined after creation. We should really find a
  better solution to this.

- It's currently only the `ref` implementation of BLAKE-256, BLAKE-512, ChaCha12
  and SPHINCS-256 that have been tested. It would be nice to have vectorized
  versions for higher performance.

- More tests :-)
