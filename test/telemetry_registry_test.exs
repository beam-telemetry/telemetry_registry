defmodule TelemetryRegistryTest do
  use ExUnit.Case

  test "multiple event attributes are persisted for Elixir modules along with docs" do
    expected_docs = """
    * `[:test_elixir_app, :event, :start]`
      * Description: emitted when this event happens
      * Measurements: `%{duration: non_neg_integer()}`
      * Metadata: `%{status: status(), name: String.t()}`

    * `[:test_elixir_app, :event, :stop]`
      * Description: emitted when this event happens
      * Measurements: `%{duration: non_neg_integer()}`
      * Metadata: `%{status: status(), name: String.t()}`

    """

    docs = TelemetryRegistry.docs_for(TestElixirApp)

    assert docs == expected_docs
    assert {:docs_v1, _, :elixir, _, %{"en" => module_doc}, _, _} = Code.fetch_docs(TestElixirApp)

    assert module_doc == """
           Information about the module.

           ## Telemetry

           #{expected_docs}
           """
  end
end
