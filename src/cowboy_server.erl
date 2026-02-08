%%
%% SIP server: gen_server + nksip behaviour
%%
-module(cowboy_server).

-export([init/2]).
-export([handle/2]).

init(Req, State) ->
    Id_String = cowboy_req:binding(id, Req, "error"),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},"Calling user",Req),
    io:format("cowboy_server: Calling user: ~p\n", [Id_String]),
    sip_server:call(Id_String),
    {ok, Req2, State}.

handle(Req, State) ->
    Id_String = cowboy_req:binding(id, Req, "error"),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},"Calling user",Req),
    io:format("Calling user: ~p\n", [Id_String]),
    {ok, Req2, State}.