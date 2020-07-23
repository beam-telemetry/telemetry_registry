-module(test_child_app).

-telemetry_event #{
                   event => [test_child_app, handler, exception],
                   description => <<"The handler raised an exception">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{kind => atom(), reason => atom(), stacktrace => term()}">>
                  }.
