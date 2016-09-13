

# SPHINCS-256 NIF for Erlang #

__Version:__ 1.0.0 (SUPERCOP: 20141124)

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

SPHINCS-256 is a high-security post-quantum stateless hash-based signature
scheme. This repository contains the SPHINCS-256 implementation found in the
SUPERCOP performance suite together with an Erlang NIF's for the SPHINCS API.

SPHINCS-256 uses 41 KB signatures, 1 KB public keys, and 1 KB private keys.

For more information about SPHINCS see:

- http://sphincs.cr.yp.to/
- http://bench.cr.yp.to/supercop.html


### <a name="Example_Usage">Example Usage</a> ###

1. Alice generates a new keypair and sends her public key to Bob.

```erlang
{ok, #{ secret := Secret, public := Public }} = sphincs:keypair().
```

2. Alice signs a document and sends it to Bob.

```erlang
SignedDocument = sphincs:sign(Document, Secret).
```

3. Bob verifies the signed document from Alice.

```erlang
sphincs:verify(SignedDocument, Public).
```


### <a name="Issues">Issues</a> ###

- It's currently only the `ref` implementation of BLAKE-256,
BLAKE-512, ChaCha12 and SPHINCS-256 that have been tested. It would be nice
to have vectorized versions for higher performance.

- The Erlang bindings of sphincs could use some tests :-)


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="sphincs.md" class="module">sphincs</a></td></tr></table>

