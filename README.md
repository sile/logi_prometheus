logi_prometheus
================

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

[Prometheus][prometheus] metrics collector for [logi].

`logi_prometheus` provides counters that count the number of log messages as follows:

```
logi_messages_total{logger="logger_name",severity="info",application="app_name",module="mod_name"} 1
```

It is useful for detecting anomalies of your application by using [alerting rules].

[logi]: https://github.com/sile/logi
[prometheus]: https://prometheus.io/
[alerting rules]: https://prometheus.io/docs/alerting/rules/

Examples
--------

Basic usage.

```erlang
%% Installs prometheus backend to the default logger
> ok = logi_prometheus_backend:install(info).

%% Logs a message
> logi:info("foo").

%% Prints metrics
> io:format(prometheus_text_format:format()).
# TYPE logi_messages_total counter
# HELP logi_messages_total Messages count
logi_messages_total{logger="logi_default_logger",severity="info",application="stdlib",module="erl_eval"} 1
```

By using [prometheus_httpd], you can easily expose metrics collected by `logi_prometheus_backend`.

[prometheus_httpd]: https://github.com/deadtrickster/prometheus-httpd

```erlang
%% Installs prometheus backend to the default logger
> ok = logi_prometheus_backend:install(info).

%% Starts metrics exporter (i.e., HTTP server)
> prometheus_httpd:start().

%% Retrieves current metrics
> httpc:request("http://localhost:8081/metrics").
```
