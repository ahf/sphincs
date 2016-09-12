

# Module sphincs #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

SPHINCS-256 NIF for Erlang.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="#type-secret_key">secret_key()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = binary()
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Generate a new SPHINCS-256 keypair.</td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Sign a message using a SPHINCS-256 secret key.</td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td>Verify a given signed message using a SPHINCS-256 public key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; {ok, KeyPair} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li><li><code>Reason = term()</code></li></ul>

Generate a new SPHINCS-256 keypair.

Generates and returns a new SPHINCS-256 keypair. The return value is a map
in order to ensure that the public key is not used as a secret key and vice
versa.

<a name="sign-2"></a>

### sign/2 ###

<pre><code>
sign(Message, SecretKey) -&gt; SignedMessage
</code></pre>

<ul class="definitions"><li><code>Message = iolist()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>SignedMessage = binary()</code></li></ul>

Sign a message using a SPHINCS-256 secret key.

Sign a given message with a SPHINCS-256 secret key. The return value is a
binary containing the signature followed by the message itself.

<a name="verify-2"></a>

### verify/2 ###

<pre><code>
verify(SignedMessage, PublicKey) -&gt; {ok, Message} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>SignedMessage = iolist()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>Message = binary()</code></li><li><code>Reason = term()</code></li></ul>

Verify a given signed message using a SPHINCS-256 public key.

Verify a given message with a SPHINCS-256 public key. The return value
contains the message itself with the signature stripped if the verification
was succesful, otherwise an error tuple is returned.

