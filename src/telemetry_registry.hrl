-ifdef('OTP_RELEASE').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Msg, Args), error_logger:error_msg(Msg, Args)).
-endif.

-ifdef('OTP_RELEASE').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_WARNING(Msg, Args), error_logger:warning_msg(Msg, Args)).
-endif.

-ifdef('OTP_RELEASE').
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

