%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(logi_prometheus_sink_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Starts application
    {ok, _} = application:ensure_all_started(logi_prometheus),
    error_logger:tty(false), % Suppresses annoying warnings

    %% Creates and registers a sink
    Sink = logi_prometheus_sink:new(default, [{registry, test_registry}]),
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
          "logi_messages_total{logger=\"default\",severity=\"info\",application=\"undefined\",module=\"logi_prometheus_sink_tests\"} 1\n\n"
         ])),

    ok.

initial_delay_test() ->
    %% Starts application
    {ok, _} = application:ensure_all_started(logi_prometheus),
    error_logger:tty(false), % Suppresses annoying warnings

    %% Creates and registers a sink
    Registry = test_registry2,
    InitialDelay = 50,
    Sink = logi_prometheus_sink:new(default, [{registry, Registry}, {initial_delay, InitialDelay}]),
    {ok, _} = logi_channel:install_sink(Sink, info),
    ?assertEqual([prometheus_counter], prometheus_registry:collectors(Registry)),

    %% Assertions
    Labels = [default, info, undefined, logi_prometheus_sink_tests],
    ?assertEqual(
       undefined,
       prometheus_counter:value(Registry, logi_messages_total, Labels)),

    logi:info("foo"),
    logi:info("bar"),
    ?assertEqual(
       0,
       prometheus_counter:value(Registry, logi_messages_total, Labels)),

    timer:sleep(InitialDelay * 2),
    ?assertEqual(
       2,
       prometheus_counter:value(Registry, logi_messages_total, Labels)),

    logi:info("baz"),
    ?assertEqual(
       3,
       prometheus_counter:value(Registry, logi_messages_total, Labels)),

    ok.
