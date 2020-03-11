# Telemetry Registry

***IMPORTANT: Experimental Library!!!***

This is an experimental library for Telemetry event discovery and registration. It is subject
to drastic changes at any time. It could eventually be published, rolled into the `telemetry`
library, or deleted.

The goal of this library is to determine a viable mechanism and patterns for event discovery
for usage in automating distributed tracing (OTel) via `telemetry` events, as well as event
documentation.

**DO NOT use this in production** but **_DO_** contribute your thoughts and feedback in the
issues!

## How It Works

The Registry works by walking your entire application tree and examining every module for
Telemetry Event definitions. 

```erlang
-telemetry_event [test_app, handler, start].
-telemetry_event [test_app, handler, failure].
-telemetry_event [test_app, handler, stop].
```

```elixir
@telemetry_event [:test_app, :handler, :start]
@telemetry_event [:test_app, :handler, :failure]
@telemetry_event [:test_app, :handler, :stop]
```

### Add the Registry to Your Application

The Registry should only be added one time and passed your application's name. Add the registry
as a child to your application's root supervision tree.

```erlang
[
    {telemetry_registry, [{application, my_app}]}
    %% other supervisor children
]
```

```elixir
[
  {:telemetry_registry, [application: :my_app]}
  # other supervisor children
]
```

### Viewing Events

The defined events can be accessed using `list_events/0`. Events are returned as a list of 
two element tuples of `{Event, Module}` where `Event` is the event name and `Module` is the
module it was discovered in.

```erlang
telemetry_registry:list_events().
```

```elixir
:telemetry_registry.list_events()
```

### Spannable Events

Tracing spans need at least a matching `start` and `stop` event to create a child span.
Optionally, a `failure` event can be emitted in the case of an exception being raised.
`spannable_events/0` returns a map of all matching (spannable) events that have been
discovered. These are returned as a map with keys being the event prefix and the value
being a list of the available events, e.g. `#{[test_app,handler] => [start,stop,failure]}`.

```erlang
telemetry_registry:spannable_events().
%% #{[test_app,handler] => [start,stop,failure]}
```

```elixir
:telemetry_registry.spannable_events()
# %{[:test_app,:handler] => [:start,:stop,:failure]}
```

## Roadmap

A few ideas that need to be tested are:

### Event Definitions

One of the biggest issues facing users is discovering what events are available in a library,
the names of them, when they are emitted, and what the measurements and metadata contain.
Measurements and metadata are probably most helpful in documentation when it resembles a
typespec.

Currently, this is all done by hand in a section the user has hopefully included in their docs.
Ideally, this could be an automatically generated section, much the same as `callbacks`, `types`,
`functions`, etc. which we currently enjoy in HexDocs.
    
Definitions could double for an event discovery mechanism, so this serves both use cases
if done well. The question is what the shape of that definition is, if it could leverage
(or just use) types for the event, and how it works in each language.
  
### Shorthand Definition

e.g. `-telemetry_events {prefix, [event1,event2]}`. This could be a separate attribute name or
macro.

