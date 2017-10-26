%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(logi_prometheus_backend_tests).
-compile({parse_transform, logi_transform}).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Starts application
    {ok, _} = application:ensure_all_started(logi_prometheus),
    error_logger:tty(false), % Suppresses annoying warnings
    ok = prometheus_registry:clear(),

    %% Installs a backend
    ok = logi_prometheus_backend:install(info),
    ?assertEqual([prometheus_counter], prometheus_registry:collectors(default)),

    %% Collects metrics (empty)
    ?assertEqual(
       prometheus_text_format:format(),
       list_to_binary(
         [
          "# TYPE logi_messages_total counter\n",
          "# HELP logi_messages_total Log messages count\n\n"
         ])),

    %% Logs a message then collects metrics
    logi:info("foo"),
    ?assertEqual(
       prometheus_text_format:format(),
       list_to_binary(
         [
          "# TYPE logi_messages_total counter\n",
          "# HELP logi_messages_total Log messages count\n"
          "logi_messages_total{logger=\"logi_default_logger\",severity=\"info\",application=\"undefined\",module=\"logi_prometheus_backend_tests\"} 1\n\n"
         ])),

    ok.
