%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A `logi' sink for collecting prometheus metrics.
%%
%% This exposes metrics that count the number of log messages sent to a channel.
%%
%% === EXAMPLE ===
%%
%% ```
%% %% Installs `metrics_sink` to the default channel.
%% > Sink = logi_prometheus_sink:new(metrics_sink, [{registry, example_registry}]).
%% > {ok, _} = logi_channel:install_sink(Sink, info).
%%
%% %% Logs a message.
%% > logi:info("foo").
%%
%% %% Prints metrics.
%% > io:format(prometheus_text_format:format(example_registry)).
%% # TYPE logi_messages_total counter
%% # HELP logi_messages_total Messages count
%% logi_messages_total{sink="metrics_sink",severity="info",application="stdlib",module="erl_eval"} 1
%% '''
-module(logi_prometheus_sink).

-behaviour(logi_sink_writer).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Id, [])
-spec new(logi_sink:id()) -> logi_sink:sink().
new(Id) ->
    new(Id, []).

%% @doc Creates a new sink instance
%%
%% === OPTIONS ===
%%
%% <b>registry</b>: <br />
%% - The prometheus registry to which metrics will be registered. <br />
%% - The default value is `default'. <br />
-spec new(logi_sink:id(), Options) -> logi_sink:sink() when
      Options :: [Option],
      Option :: {registry, atom()|string()}.
new(Id, Options) ->
    Registry = proplists:get_value(registry, Options, default),
    prometheus_counter:declare(
      [
       {name, logi_messages_total},
       {help, "Messages count"},
       {labels, [sink, severity, application, module]},
       {registry, Registry}
      ]),
    logi_sink:from_writer(Id, logi_sink_writer:new(?MODULE, {Id, Registry})).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, _Format, _Data, {SinkId, Registry}) ->
    Location = logi_context:get_location(Context),
    Labels =
        [
         SinkId,
         logi_context:get_severity(Context),
         logi_location:get_application(Location),
         logi_location:get_module(Location)
        ],
    prometheus_counter:inc(Registry, logi_messages_total, Labels, 1),
    [].

%% @private
get_writee(_) -> undefined.
