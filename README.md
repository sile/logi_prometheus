logi_prometheus
================

[![hex.pm version](https://img.shields.io/hexpm/v/logi_prometheus.svg)](https://hex.pm/packages/logi_prometheus)
[![Build Status](https://travis-ci.org/sile/logi_prometheus.svg?branch=master)](https://travis-ci.org/sile/logi_prometheus)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

[Prometheus][prometheus] metrics collector for [logi].

[Documentation](https://hexdocs.pm/logi_prometheus/)

`logi_prometheus` provides counters that count the number of log messages as follows:

```
logi_messages_total{sink="sink_name",severity="info",application="app_name",module="mod_name"} 1
```

It is useful for detecting anomalies of your application by using [alerting rules].

[logi]: https://github.com/sile/logi
[prometheus]: https://prometheus.io/
[alerting rules]: https://prometheus.io/docs/alerting/rules/

Examples
--------

Basic usage.

```erlang
%% Installs `metrics_sink` to the default channel
> Sink = logi_prometheus_sink:new(metrics_sink, [{registry, example_registry}]).
> {ok, _} = logi_channel:install_sink(Sink, info).

%% Logs a message
> logi:info("foo").

%% Prints metrics
> io:format(prometheus_text_format:format(example_registry)).
# TYPE logi_messages_total counter
# HELP logi_messages_total Messages count
logi_messages_total{sink="metrics_sink",severity="info",application="stdlib",module="erl_eval"} 1
```

By using [prometheus_httpd], you can easily expose metrics collected by `logi_prometheus_sink`.

[prometheus_httpd]: https://github.com/deadtrickster/prometheus-httpd

```erlang
%% Installs `metrics_sink` to the default channel
> Sink = logi_prometheus_sink:new(metrics_sink).
> {ok, _} = logi_channel:install_sink(Sink, info).

%% Starts metrics exporter (i.e., HTTP server)
> prometheus_httpd:start().

%% Retrieves current metrics
> httpc:request("http://localhost:8081/metrics").
```
