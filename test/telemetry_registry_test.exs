defmodule TelemetryRegistryTest do
  use ExUnit.Case

  test "multiple event attributes are persisted for Elixir modules along with docs" do
    expected_docs = """
    * `[:test_elixir_app, :single, :event]`
      * Description: emitted when this event happens
      * Measurements: `%{duration: non_neg_integer()}`
      * Metadata: `%{status: status(), name: String.t()}`

    """

    docs = TelemetryRegistry.docs_for(TestElixirApp)

    assert docs == expected_docs
  end
end
