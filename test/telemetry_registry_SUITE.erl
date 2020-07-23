-module(telemetry_registry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() -> [
  discovers_all_events,
  determines_spannable_events,
  supports_event_definitions
].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(telemetry_registry),
    application:ensure_all_started(test_app),
    Config.

end_per_suite(_Config) ->
    application:stop(test_app),
    application:stop(telemetry_registry).

discovers_all_events(_Config) ->
    telemetry_registry:discover_all(),
    Events = telemetry_registry:list_events(),
    ?assertEqual(4, erlang:length(Events)).

determines_spannable_events(_Config) ->
    telemetry_registry:discover_all(),
    Events = telemetry_registry:spannable_events(),
    ?assertEqual([
              {[test_app,handler], [start,stop,exception]}
             ], Events).

supports_event_definitions(_Config) ->
    telemetry_registry:discover_all(),
    Events = telemetry_registry:list_events(),
    Event = [test_app, handler, stop],
    {Event, _, #{
      description := Description,
      measurements := Measurements,
      metadata := Metadata
     }} = lists:keyfind(Event, 1, Events),
    ?assert(is_binary(Description)),
    ?assert(is_binary(Measurements)),
    ?assert(is_binary(Metadata)).
