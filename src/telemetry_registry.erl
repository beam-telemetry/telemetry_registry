-module(telemetry_registry).

%% API
-export([discover_all/0,
         discover_all/1,
         list_events/0,
         spannable_events/0]).

-define(KEY, ?MODULE).

-type application() :: atom().

list_events() ->
    persistent_term:get(?KEY, []).

spannable_events() ->
    AllEvents = list_events(),
    ReversedEvents = reverse_events(AllEvents),
    Filtered = filter_events(ReversedEvents),
    {_, Spans} = event_spans(Filtered),
    Spans.

reverse_events(Events) ->
    [lists:reverse(Event) || {Event, _} <- Events].

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

-spec discover_all() -> ok.
discover_all() ->
    discover_all([App || {App, _, _} <- application:loaded_applications()]).

-spec discover_all(application() | [application()]) -> ok.
discover_all(Applications) ->
    Events = maps:keys(maps:from_list([{Event, []} || Event <- gather_all_events(Applications)])),
    persistent_term:put(?KEY, Events).

gather_all_events(Applications) when is_list(Applications) ->
    lists:flatmap(fun gather_all_events/1, Applications);
gather_all_events(Application) ->
    Applications = child_applications(Application),
    lists:flatmap(fun gather_application_events/1, [Application | Applications]).

child_applications(Application) ->
    {ok, ChildApplications} = application:get_key(Application, applications),
    {ok, ChildIncludedApplications} = application:get_key(Application, included_applications),
    ChildApplications ++ ChildIncludedApplications.

gather_application_events(Application) ->
    case application:get_key(Application, modules) of
        {ok, Modules} -> lists:flatmap(fun events_for_module/1, Modules);
        _ -> []
    end.

-spec events_for_module(atom()) -> [telemetry:event_name()].
events_for_module(Module) ->
    Info = Module:module_info(),
    Attributes = proplists:get_value(attributes, Info),
    Events = proplists:get_all_values(telemetry_event, Attributes),
    [{Event, Module} || Event <- Events].
