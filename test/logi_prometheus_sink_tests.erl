%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(logi_prometheus_sink_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Starts application
    {ok, _} = application:ensure_all_started(logi_prometheus),
    error_logger:tty(false), % Suppresses annoying warnings

    %% Creates and registers a sink
    Sink = logi_prometheus_sink:new(test_sink, [{registry, test_registry}]),
    {ok, _} = logi_channel:install_sink(Sink, info),
    ?assertEqual([prometheus_counter], prometheus_registry:collectors(test_registry)),

    %% Collects metrics (empty)
    ?assertEqual(
       prometheus_text_format:format(test_registry),
       list_to_binary(
         [
          "# TYPE logi_messages_total counter\n",
          "# HELP logi_messages_total Messages count\n\n"
         ])),

    %% Logs a message then collects metrics
    logi:info("foo"),
    ?assertEqual(
       prometheus_text_format:format(test_registry),
       list_to_binary(
         [
          "# TYPE logi_messages_total counter\n",
          "# HELP logi_messages_total Messages count\n"
          "logi_messages_total{sink=\"test_sink\",severity=\"info\",application=\"undefined\",module=\"logi_prometheus_sink_tests\"} 1\n\n"
         ])),

    ok.