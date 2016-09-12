%%%
%%% Copyright (c) 2016 Alexander Færøy. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for Sphincs
%%% @end
%%% -----------------------------------------------------------
-module(prop_sphincs).

-include_lib("proper/include/proper.hrl").

prop_sign_verify() ->
    ?FORALL({{Public, Secret}, Message}, {keypair(), binary()},
        begin
            SignedMessage = sphincs:sign(Message, Secret),
            {ok, VerifiedMessage} = sphincs:verify(SignedMessage, Public),
            Message =:= VerifiedMessage
        end).

%% @private
keypair() ->
    {ok, #{ secret := Secret,
            public := Public }} = sphincs:keypair(),
    {Public, Secret}.
