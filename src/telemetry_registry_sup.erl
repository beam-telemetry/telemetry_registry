%% @private
-module(telemetry_registry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Opts) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    TableHandler = #{id => telemetry_registry_table,
                     start => {telemetry_registry_table, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [telemetry_registry_table]},
    {ok, {SupFlags, [TableHandler]}}.

%%====================================================================
%% Internal functions
%%====================================================================
