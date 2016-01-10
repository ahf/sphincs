-module(sphincs).

%% API.
-export([keypair/0,
         sign/2,
         verify/2]).

-ifdef(TEST).
-include_lib("triq/include/triq.hrl").
-endif.

-type secret_key() :: binary().
-type public_key() :: binary().

-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

-spec keypair() -> keypair().
keypair() ->
    {Public, Secret} = sphincs_nif:keypair(),
    #{ public => Public, secret => Secret }.

-spec sign(Message :: binary(), Secret :: secret_key()) -> binary().
sign(Message, Secret) when is_binary(Message), is_binary(Secret) ->
    sphincs_nif:sign(Message, Secret).

-spec verify(Message :: binary(), Public :: public_key()) -> {ok, binary()} | {error, term()}.
verify(SignedMessage, Public) when is_binary(SignedMessage), is_binary(Public) ->
    case sphincs_nif:verify(SignedMessage, Public) of
        Message when is_binary(Message) ->
            {ok, Message};

        {error, _} = Error ->
            Error
    end.

-ifdef(TEST).
key() ->
    #{ secret := Secret, public := Public } = keypair(),
    {Public, Secret}.

prop_create_sign_verify_valid() ->
    ?FORALL({{Public, Secret}, Message}, {key(), binary()},
        begin
            SignedMessage = sign(Message, Secret),
            {ok, VerifiedMessage} = verify(SignedMessage, Public),
            Message =:= VerifiedMessage
        end).
-endif.
