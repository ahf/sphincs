%%%
%%% Copyright (c) 2016 Alexander Færøy. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc SPHINCS-256 NIF for Erlang
%%% @end
%%% -----------------------------------------------------------
-module(sphincs).

%% API.
-export([keypair/0,
         sign/2,
         verify/2]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: binary().
-type public_key() :: binary().

-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

%% @doc Generate a new SPHINCS-256 keypair.
%%
%% Generates and returns a new SPHINCS-256 keypair. The return value is a map
%% in order to ensure that the public key is not used as a secret key and vice
%% versa.
%%
%% @end
-spec keypair() -> {ok, KeyPair} | {error, Reason}
    when
        KeyPair :: keypair(),
        Reason  :: term().
keypair() ->
    case sphincs_nif:keypair(crypto:strong_rand_bytes(1088)) of
        {error, _} = Error ->
            Error;

        {Public, Secret} ->
            {ok, #{ public => Public,
                    secret => Secret }}
    end.

%% @doc Sign a message using a SPHINCS-256 secret key.
%%
%% Sign a given message with a SPHINCS-256 secret key. The return value is a
%% binary containing the signature followed by the message itself.
%%
%% @end
-spec sign(Message, SecretKey) -> SignedMessage
    when
        Message       :: iolist(),
        SecretKey     :: secret_key(),
        SignedMessage :: binary().
sign(Message, Secret) when is_binary(Message), is_binary(Secret) ->
    sphincs_nif:sign(Message, Secret).

%% @doc Verify a given signed message using a SPHINCS-256 public key.
%%
%% Verify a given message with a SPHINCS-256 public key. The return value
%% contains the message itself with the signature stripped if the verification
%% was succesful, otherwise an error tuple is returned.
%%
%% @end
-spec verify(SignedMessage, PublicKey) -> {ok, Message} | {error, Reason}
    when
        SignedMessage :: iolist(),
        PublicKey     :: public_key(),
        Message       :: binary(),
        Reason        :: term().
verify(SignedMessage, Public) when is_binary(SignedMessage), is_binary(Public) ->
    case sphincs_nif:verify(SignedMessage, Public) of
        Message when is_binary(Message) ->
            {ok, Message};

        {error, _} = Error ->
            Error
    end.
