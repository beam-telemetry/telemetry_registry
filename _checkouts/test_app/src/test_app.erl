-module(test_app).

-telemetry_event #{
                   event => [test_app, handler, start],
                   description => <<"Emitted at the start of the handler">>,
                   measurements => <<"#{system_time => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_app, handler, stop],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_app, handler, exception],
                   description => <<"The handler raised an exception">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{kind => atom(), reason => atom(), stacktrace => term()}">>
                  }.

-telemetry_event [test_app, only, stop].

-telemetry_event [test_app, cache, miss].
-telemetry_event [test_app, cache, hit].
