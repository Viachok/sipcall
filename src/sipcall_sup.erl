%%%-------------------------------------------------------------------
%% @doc sipcall top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sipcall_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([dump/0]).
-export([print_tree/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        nksip:get_sup_spec(sip_server, #{
        sip_local_host => "localhost",
        plugins => [nksip_registrar],
        sip_listen => "sip:all:5060"
        }),
        %% SIP-клиент для звонка
        nksip:get_sup_spec(sip_client, #{
            sip_local_host => "localhost",
            sip_from => "sip:sip_client@127.0.0.1",
            plugins => [nksip_uac_auto_auth],
            sip_listen => "sip:127.0.0.1:5075"
        })
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

dump() ->
    io:format("++++ Supervisor tree: sipcall_sup ~p~n", [whereis(sipcall_sup)]),
    print_tree(whereis(sipcall_sup), sipcall_sup, 0),
    io:format("~n+++ ~p~n", [sys:get_state(whereis(sip_server))]).
    %%io:format("~n+++ ~p~n", [sys:get_status(whereis(cowboy_server))]).

print_tree(Proc, ProcId, Level) ->
    RegName = erlang:process_info(Proc, registered_name),
    case catch supervisor:which_children(Proc) of
        {'EXIT', _} ->
            indent(Level),
            io:format("+-- ~p ~p <worker> ~p~n", [Proc, ProcId, RegName]);
        Children when is_list(Children) ->
            indent(Level),
            io:format("+-- ~p ~p <supervisor> ~p~n", [Proc, ProcId, RegName]),
            lists:foreach(
                fun({Id, Child, Type, Modules}) ->
                    print_child(Id, Child, Type, Modules, Level + 1)
                end,
                Children
            )
    end.

print_child(Id, Child, supervisor, _Modules, Level) ->
    print_tree(Child, Id, Level);

print_child(Id, Child, worker, Modules, Level) ->
    indent(Level),
    RegName = erlang:process_info(Child, registered_name),
    io:format("+-- ~p ~p (worker, modules: ~p) ~p~n", [Child, Id, Modules, RegName]).

indent(Level) ->
    lists:foreach(
        fun(_) -> io:format("|    ") end,
        lists:seq(1, Level)
    ).