-module(telemetry_registry).

-behaviour(gen_server).

%% API
-export([
    discover_all/1,
    list_events/0,
    register_application_events/1,
    spannable_events/0]).

-export([start_link/1, child_spec/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("telemetry_registry.hrl").

-type application() :: atom().
-type option() :: {name, gen_server:name()} | {application, application()}.

-spec start_link([option()]) -> gen_server:on_start().
start_link(Opts) ->
    Args = parse_args(Opts),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec init(map()) -> {ok, map()}.
init(Opts) ->
    discover_all(maps:get(application, Opts)),
    {ok, #{}}.

%% @doc
%% Returns a child spec for the poller for running under a supervisor.
child_spec(Opts) ->
    #{id => ?MODULE, start => {telemetry_registry, start_link, [Opts]}}.

parse_args(Args) ->
    Application = proplists:get_value(application, Args),
    validate_application(Application),
    #{application => Application}.

validate_application(Application) when is_atom(Application) ->
    case application:get_application(Application) of
      {ok, _AppInfo} ->
          ok;
      undefined ->
          erlang:error({badarg, "The application could not be found", [Application]})
    end;
validate_application(Term) ->
    erlang:error({badarg, "Expected application to be an atom", [Term]}).

list_events() ->
    Events = telemetry_registry_table:list_all(),
    logger:error("events", [Events]),
    Events.

spannable_events() ->
    AllEvents = telemetry_registry_table:list_events(),
    ReversedEvents = reverse_events(AllEvents),
    Filtered = filter_events(ReversedEvents),
    {_, Spans} = event_spans(Filtered),
    Spans.

reverse_events(Events) ->
    lists:foldl(fun([Event],Reversed) -> [lists:reverse(Event) | Reversed] end, [], Events).

filter_events(Events) ->
    lists:filter(fun([Suffix | _]) -> lists:member(Suffix, [start, stop, failure]) end, Events).

event_spans(Events) ->
    lists:foldl(fun([start | Event], {AllEvents, Spans}) ->
                        case lists:member([stop | Event], AllEvents) of
                            true ->
                                SpanPrefix = lists:reverse(Event),
                                case lists:member([failure | Event], AllEvents) of
                                    true ->
                                        UpdatedSpans = maps:put(SpanPrefix, [start, stop, failure], Spans),
                                        {AllEvents, UpdatedSpans};
                                    false ->
                                        {Events, maps:put(SpanPrefix, [start, stop], Spans)}
                                end;
                            false ->
                                {AllEvents, Spans}
                        end;
                   (_, Acc) ->
                        Acc
    end, {Events, #{}}, Events).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

discover_all(Application) ->
    ok = register_application_events(Application),
    Applications = child_applications(Application),
    ok = lists:foreach(fun discover_all/1, Applications).

child_applications(Application) ->
    {ok, ChildApplications} = application:get_key(Application, applications),
    {ok, ChildIncludedApplications} = application:get_key(Application, included_applications),
    lists:flatten([ChildApplications, ChildIncludedApplications]).

register_application_events(Application) ->
    {ok, Modules} = application:get_key(Application, modules),
    lists:foreach(fun (Module) ->
                          register_module_events(Module, Application)
                  end,
                  Modules).

register_module_events(Module, _Application) ->
    Events = events_for_module(Module),
    lists:foreach(fun (Event) ->
                          telemetry_registry_table:insert(Event, Module)
                  end,
                  Events).

%% {event, application, module, meta}

-spec events_for_module(atom()) -> [telemetry:event_name()].
events_for_module(Module) ->
    Info = Module:module_info(),
    Attributes = proplists:get_value(attributes, Info),
    proplists:get_all_values(telemetry_event, Attributes).


