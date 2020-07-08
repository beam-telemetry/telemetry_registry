defmodule TelemetryRegistry do
  @moduledoc """
  TelemetryRegistry provides tools for the discovery and documentation of [telemetry](https://github.com/beam-telemetry/telemetry)
  events within your applications.

  ## Telemetry Event Definitions and Declaration

  Users want to know what telemetry events are available in your library, what they mean, as well as what
  the measurements and metadata maps contain. TelemetryRegistry creates an official standard and mechanism
  for telemetry event declaration and optional definition. It is _highly_ encouraged to use the definition
  format for your users.

  ### Who Should Document Events?

  Library authors in particular should provide event declarations for documentation and to simplify tracing.
  Of course, everyone should document their modules!

  ### Where Should I Declare Events?

  Events should only be declared once, usually in the module from which it originates. Event names should _always_
  be namespaced to your application or library. For example, if your application is an http client, your events
  should start with the name of your application, not a generic name.

  **Do:** `[:tesla, :request, :stop]`
  **Don't:** `[:http_client, :request, :stop]`

  ### Event Definition Format

  Events are declared using the `telemetry_event` module attribute. The attribute accepts an event definition
  or a plain event, though plain events are discouraged. These definitions are used for producing documentation
  and event discovery. All definition keys are required.

  ```elixir
  %{
    event: [:my_app, :event, :stop],
    description: "A description of what the event is and when it is emitted",
    measurements: "A string containing a pseudo or typespec - see examples",
    metadata: "A string containing a pseudo or real typespec - see examples"
  }
  ```

  ```erlang
  #{
    event => [my_app, event, stop],
    description => <<"A description of what the event is and when it is emitted">>,
    measurements => <<"A string containing a pseudo or typespec - see examples">>,
    metadata => <<"A string containing a pseudo or real typespec - see examples">>
  }
  ```

  #### Elixir
  
  Elixir does not allow for declaring a custom attribute multiple times by default. We have included macros
  to help with this and to provide a way to include event documentation.

  ```elixir
  defmodule TestElixirApp do
    use TelemetryRegistry

    telemetry_event(%{
      event: [:test_elixir_app, :single, :event],
      description: "emitted when this event happens",
      measurements: "%{duration: non_neg_integer()}",
      metadata: "%{status: status(), name: String.t()}"
    })

    telemetry_event([:test_elixir_app, :event, :stop])
  end
  ```

  Add `use TelemetryRegistry` at the top of your module to prep your module for defining events. This
  handles setting up everything needed to declare events and the very helpful `telemetry_event/1`
  macro.

  ### Event Documentation

  Elixir users can take advantage of automatic documentation formatting in your moduledoc.

  ```
  ## Telemetry

  #{format_telemetry_docs(@telemetry_docs)}
  ```

  ### Event Discovery

  Events can be discovered by invoking `discover_all`, usually during application startup. The registry
  will walk through the application tree and aggregate all events. The events are cached, so this should
  only be invoked once at startup. You can view all declared events using `list_events/0`. It is also possible
  to limit event discovery to a particular application tree by passing an application name to `discover_all/1`.

  ## Distributed Tracing

  Event discovery is critical for supporting distributed tracing of black-box libraries used
  in your application. Library authors are encouraged to use telemetry events in their libraries to provide
  insight of internal operations to users in a vendor-agnostic manner.

  TelemetryRegistry provides a mechanism through `spannable_events/0` for distributed tracing library authors
  to discover events which can be used to start and stop child spans by registering telemetry event handlers
  automatically at runtime with no user intervention. Library authors can then provide additional mechanisms
  for users to enhance spans with attributes created from telemetry event measurements and metadata.
  """

  @typedoc """
  An application to discover events from.
  """
  @type application() :: :telemetry_registry.application()

  @typedoc """
  A tuple containing the telemetry event, the module in which it was declared, and event definition meta.
  `{:telemetry.event_name(), module(), t:event_meta()`
  """
  @type event() :: :telemetry_registry.event()

  @typedoc """
  An event definition is composed of an event, description, measurements description, and metadata description.
  """
  @type event_definition() :: :telemetry_registry.event_definition()

  @typedoc """
  A description of what the event represents and when it is emitted.
  """
  @type event_description() :: :telemetry_registry.event_description()

  @typedoc """
  A string representation of the measurements emitted with the event. This should resemble a typespec but is
  not limited to the typespec format, i.e. you can include a comment on which unit a value is in. The objective
  is to inform users.
  """
  @type event_measurements() :: :telemetry_registry.event_measurements()

  @typedoc """
  A string representation of the metadata emitted with the event. This should resemble a typespec but is
  not limited to the typespec format, i.e. you can include comments or example values. The objective
  is to inform users what is available.
  """
  @type event_metadata() :: :telemetry_registry.event_metadata()

  @typedoc """
  A map of event definition meta for an event containing the event, measurements, and metadata descriptions
  if the event was declared with an event definition. Otherwise, this value will be an empty map.
  """
  @type event_meta() :: :telemetry_registry.event_meta()

  @typedoc """
  A list of spannable events known to the registry in the format of `{event_prefix, event_suffixes}`. For
  example, given events `[:my_app, :request, :start], [:my_app, :request, :stop], [:my_app, :request, :exception]`
  a span can be created from the `:start` -> `:stop` or the `:start` -> `:exception` events. These are aggregated
  as a spannable event `{[:my_app, :request], [:start, :stop, :exception]}`.
  """
  @type spannable_event() :: :telemetry_registry.spannable_event()


  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__), only: [format_telemetry_docs: 1, telemetry_event: 1]

      Module.register_attribute(__MODULE__, :telemetry_event,
        accumulate: true,
        persist: true
      )

      Module.register_attribute(__MODULE__, :telemetry_docs,
        accumulate: true,
        persist: true
      )
    end
  end

  @doc """
  Declares a telemetry event. Accepts a telemetry event definition or telemetry event name. A full
  definition is required for complete documentation and is highly encouraged.
  """
  defmacro telemetry_event(event) do
    docstring =
      case event do
        event when is_list(event) ->
          """
          * `#{inspect(event)}`

          """

        {:%{}, _, event} ->
          """
          * `#{inspect(event[:event])}`
            * Description: #{event[:description]}
            * Measurements: `#{event[:measurements]}`
            * Metadata: `#{event[:metadata]}`

          """
      end

    quote do
      @telemetry_event unquote(event)
      @telemetry_docs unquote(docstring)
    end
  end

  @doc """
  Formats telemetry definitions declared in the module for inclusion in documentation.

  Example

  ```
  ## Telemetry

  #{format_telemetry_docs(@telemetry_docs)}
  ```
  """
  @spec format_telemetry_docs([String.t()]) :: String.t()
  def format_telemetry_docs(docs), do: Enum.reverse(docs) |> IO.iodata_to_binary()

  @doc """
  Discover all declared telemetry events in the application it is invoked from and all child applications.
  This would normally be invoked during application startup.
  """
  @spec discover_all() :: :ok
  defdelegate discover_all(), to: :telemetry_registry

  @doc """
  Discover all declared telemetry events in the given application and its child applications. This is
  typically used in libraries leveraging `telemetry_registry` where it would be necessary for the user
  to define what the root application is, e.g. in tracing bridge libraries.
  """
  @spec discover_all(application()) :: :ok
  defdelegate discover_all(application), to: :telemetry_registry

  @doc """
  Returns a list of all registered events.

  Example

  ```
    iex> TelemetryRegistry.list_events()
    [{[:my_app, :request, :stop], %{description: "Event description", measurements: "Measurements description, metadata: "Metadata description"}}]
  ```
  """
  @spec list_events() :: [event()]
  defdelegate list_events(), to: :telemetry_registry

  @doc """
  Returns a list of spannable events.

  Example

  ```
    iex> TelemetryRegistry.spannable_events()
    [{[:my_app, :request], [:start, :stop, :exception]}]
  ```
  """
  @spec spannable_events() :: [spannable_event()]
  defdelegate spannable_events(), to: :telemetry_registry
end