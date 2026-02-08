%%
%% SIP server: gen_server + nksip behaviour
%%
-module(sip_server).

-export([sip_authorize/3, sip_route/5, sip_register/2, sip_invite/2, call/1]).

-include_lib("nkserver/include/nkserver_module.hrl").
%% SIP callbacks: https://github.com/NetComposer/nksip/blob/master/doc/reference/callback_functions.md

call(UserName) ->
    FoundedUser = find_user_no_auth(UserName),
    case FoundedUser of
        [] -> {error,nonregistereduser};
        _-> [Map] = FoundedUser,
            StringURI = maps:get(<<"userURI">>,Map),
            {ok, Tokens, _} = erl_scan:string(StringURI ++ "."),
            {ok, URI} = erl_parse:parse_term(Tokens),
            sip_client:call(URI)
    end.


%% Called to check the user password for a realm
%sip_get_user_pass(User, _Realm, _Req, _Call) ->
%    io:format("sip_server: sip_get_user_pass(~p)~n", [User]),
%    find_pass_userauth(User).

%% Called for every incoming request to be authorized or not
sip_authorize(_AuthList, Req, _Call) ->
    io:format("sip_server: sip_authorize()~n"),
    FromUser = nksip_sipmsg:get_meta(from_user, Req),
    URI = nksip_sipmsg:get_meta(contacts, Req),
    FoundedUser = find_user_no_auth(FromUser),
    io:format("sip_server: ~p ~p~n",[FoundedUser, FromUser]),
    case FoundedUser of
        [] -> register_user(FromUser, URI);
        _ -> ok
    end.

%% This function is called by NkSIP for every new request, to check if it must be proxied, processed locally or replied immediately
sip_route(_Scheme, <<>>, <<"localhost">>, _Req, _Call) ->
    % we want to act as an endpoint or B2BUA
    io:format("sip_server: sip_route(User = <<>>)~n"),
    process;

sip_route(_Scheme, User, _Domain, Req, _Call) ->
    io:format("sip_server: sip_route(User = ~p)~n", [User]),
    case nksip_request:is_local_ruri(Req) of
        true ->
            process;
        false ->
            proxy
    end.

%% This function is called by NkSIP to process a new incoming REGISTER request
sip_register(Req, _Call) ->
    {ok, [{from_scheme, FromScheme}, {from_user, FromUser}, {from_domain, FromDomain}]} =
        nksip_request:get_metas([from_scheme, from_user, from_domain], Req),
    {ok, [{to_scheme, ToScheme}, {to_user, ToUser}, {to_domain, ToDomain}]} =
        nksip_request:get_metas([to_scheme, to_user, to_domain], Req),

    io:format("sip_server: sip_register(From ~p)~n", [FromUser]),
    case {FromScheme, FromUser, FromDomain} of
        {ToScheme, ToUser, ToDomain} ->
            io:format("REGISTER OK: ~p~n", [{ToUser, ToDomain}]),
            {reply, nksip_registrar:request(Req)};
        _ ->
            {reply, forbidden}
    end.

%% This function is called by NkSIP to process a new INVITE request as an endpoint
sip_invite(Req, _Call) ->
    {ok, [{scheme, Scheme}, {from_user, FromUser}, {user, User}, {domain, Domain}]} =
        nksip_request:get_metas([scheme, from_user, user, domain], Req),

    io:format("sip_server: sip_invite(From ~p, User ~p)~n", [FromUser, User]),
    case nksip_registrar:find(?MODULE, Scheme, FromUser, Domain) of
        [] ->
            {reply, forbidden};
        _UriList ->
            {ok, Body} = nksip_request:body(Req),
            case nksip_sdp:is_sdp(Body) of
                true ->
                    %Contact = nksip_sipmsg:get_meta(contacts, Req),
                    % Планируем ответный звонок клиенту через 10 секунд
                    %io:format("sip_server: schedule callback to ~p~n", [Contact]),
                    %schedule_callback(Contact),
                    % Ответ клиенту retry-after (487), чтоб перезвонить абоненту
                    {reply, {487, []}};
                false ->
                    {reply, forbidden}
            end
    end.

%% Private functions

get_users_from_json() ->
    {ok, Binary} = file:read_file("users/users.json"),
    Output = jsx:decode(Binary),
    #{<<"users">> := Users} = Output,
    Users.

find_user_no_auth(UserFrom) ->
    Users = get_users_from_json(),
    Filter = fun(UserMap) ->
        UserName = maps:get(<<"userName">>, UserMap),
        _UserURI = maps:get(<<"userURI">>, UserMap),
        true andalso UserName == UserFrom
    end,
    lists:filter(Filter, Users).

register_user(UserFrom, URI) ->
    {ok, Binary} = file:read_file("users/users.json"),
    Output = jsx:decode(Binary),
    Users = maps:get(<<"users">>, Output),
    NewData = [#{<<"userName">> => UserFrom,<<"userURI">> => lists:flatten(io_lib:format("~w", URI))}],
    CombinedData = lists:append(Users,NewData),
    EmptyMap = maps:new(),
    NewMap = maps:put(<<"users">>, CombinedData, EmptyMap),
    NewBinary = jsx:encode(NewMap),
    file:write_file("users/users.json", NewBinary),
    io:format("sip_server: user remembered~n").

