-module(test_child_app).

-telemetry_event [test_child_app, extra_long, handler, start].
-telemetry_event [test_child_app, extra_long, handler, stop].

-telemetry_event [test_child_app, cache, miss].
-telemetry_event [test_child_app, cache, hit].
