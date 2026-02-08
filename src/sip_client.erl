%%
%% SIP client
%%
-module(sip_client).

-export([call/1]).

-include_lib("nkserver/include/nkserver_module.hrl").
%% SIP callbacks: https://github.com/NetComposer/nksip/blob/master/doc/reference/callback_functions.md

call(TargetUri) ->
    io:format("sip_client: calling back to client ~p~n", [TargetUri]),
    nksip_uac:invite(sip_client, TargetUri, [auto_2xx_ack]),
    ok.
