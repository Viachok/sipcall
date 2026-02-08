%%%-------------------------------------------------------------------
%% @doc sipcall public API
%% @end
%%%-------------------------------------------------------------------

-module(sipcall_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/api/call/:id", cowboy_server, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    Res = sipcall_sup:start_link(),
    sipcall_sup:dump(),
    Res.

stop(_State) ->
    ok.

%% internal functions
