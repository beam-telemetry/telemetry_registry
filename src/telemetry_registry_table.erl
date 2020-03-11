%%%-------------------------------------------------------------------
%% @private ETS table for events.
%%
%% Each handler is stored in the table. A key is an event name the
%% handler is attached to. All writes to a table go through a single
%% Agent process to make sure that we don't get duplicate handler IDs.
%%
%% Reads (`list_handlers_...') are executed by the calling process.
%% @end
%%%-------------------------------------------------------------------
-module(telemetry_registry_table).

-behaviour(gen_server).

-export([start_link/0,
         insert/2,
         list_all/0,
         list_events/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("telemetry_registry.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec insert(EventName, Module) -> ok | {error, already_exists} when
      EventName :: telemetry:event_name(),
      Module :: module().
insert(EventName, Module) ->
    gen_server:call(?MODULE, {insert, EventName, Module}).

list_all() ->
    ets:tab2list(?MODULE).

list_events() ->
    ets:match(?MODULE, {'$1','_'}).

init([]) ->
    _ = create_table(),
    {ok, []}.

handle_call({insert, EventName, Module}, _From, State) ->
    case ets:match(?MODULE, {EventName, Module}) of
        [] ->
            ets:insert(?MODULE, {EventName, Module}),
            {reply, ok, State};
        _ ->
            {reply, {error, already_exists}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

create_table() ->
    ets:new(?MODULE, [public, named_table]).

