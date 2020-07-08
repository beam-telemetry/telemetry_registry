defmodule TelemetryRegistryTest do
  use ExUnit.Case

  test "multiple event attributes are persisted for Elixir modules along with docs" do
    attrs = TestElixirApp.__info__(:attributes)
    events = Enum.filter(attrs, &match?({:telemetry_event, _}, &1))
    assert length(events) == 2

    event_docs =
      Enum.filter(attrs, &match?({:telemetry_docs, _}, &1))
      |> Enum.map(&elem(&1, 1))
      |> Enum.reverse()

    assert length(event_docs) == 2

    expected_docs = """
    * `[:test_elixir_app, :event, :stop]`

    * `[:test_elixir_app, :single, :event]`
      * Description: emitted when this event happens
      * Measurements: `%{duration: non_neg_integer()}`
      * Metadata: `%{status: status(), name: String.t()}`

    """

    assert TelemetryRegistry.format_telemetry_docs(event_docs) == expected_docs
  end
end
