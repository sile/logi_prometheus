logi_prometheus
================

[![hex.pm version](https://img.shields.io/hexpm/v/logi_prometheus.svg)](https://hex.pm/packages/logi_prometheus)
[![Build Status](https://travis-ci.org/sile/logi_prometheus.svg?branch=master)](https://travis-ci.org/sile/logi_prometheus)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

[Prometheus][prometheus] metrics collector for [logi][logi] powered by [prometheus.erl][prometheus.erl].

[Documentation](https://hexdocs.pm/jsone/)

[logi]: https://github.com/sile/logi
[prometheus]: https://prometheus.io/
[prometheus.erl]: https://github.com/deadtrickster/prometheus.erl

Examples
--------

```erlang
%% Installs `metrics_sink` to the default channel.
> Sink = logi_prometheus_sink:new(metrics_sink, [{registry, example_registry}]).
> {ok, _} = logi_channel:install_sink(Sink, info).

%% Logs a message.
> logi:info("foo").

%% Prints metrics.
> io:format(prometheus_text_format:format(example_registry)).
# TYPE logi_messages_total counter
# HELP logi_messages_total Messages count
logi_messages_total{sink="metrics_sink",severity="info",application="stdlib",module="erl_eval"} 1
```
