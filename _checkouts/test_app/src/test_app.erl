-module(test_app).

-telemetry_event [test_app, handler, start].
-telemetry_event [test_app, handler, stop].
-telemetry_event [test_app, handler, exception].

-telemetry_event [test_app, only, stop].

-telemetry_event [test_app, cache, miss].
-telemetry_event [test_app, cache, hit].
