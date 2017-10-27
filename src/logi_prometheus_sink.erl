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
-spec new(logi:logger_id()) -> logi_sink:sink().
new(LoggerId) ->
    new(LoggerId, []).

%% @doc Creates a new sink instance
%%
%% === OPTIONS ===
%%
%% <b>sink_id</b>: <br />
%% - The identifier of this sink. <br />
%% - The default value is `logi_prometheus_sink'. <br />
%%
%% <b>registry</b>: <br />
%% - The prometheus registry to which metrics will be registered. <br />
%% - The default value is `default'. <br />
-spec new(logi:logger_id(), Options) -> logi_sink:sink() when
      Options :: [Option],
      Option :: {sink_id, logi_sink:id()} |
                {registry, atom()|string()}.
new(LoggerId, Options) ->
    SinkId = proplists:get_value(sink_id, Options, ?MODULE),
    Registry = proplists:get_value(registry, Options, default),
    prometheus_counter:declare(
      [
       {name, logi_messages_total},
       {help, "Messages count"},
       {labels, [logger, severity, application, module]},
       {registry, Registry}
      ]),
    logi_sink:from_writer(SinkId, logi_sink_writer:new(?MODULE, {LoggerId, Registry})).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, _Format, _Data, {LoggerId, Registry}) ->
    Location = logi_context:get_location(Context),
    Labels =
        [
         LoggerId,
         logi_context:get_severity(Context),
         logi_location:get_application(Location),
         logi_location:get_module(Location)
        ],
    prometheus_counter:inc(Registry, logi_messages_total, Labels, 1),
    [].

%% @private
get_writee(_) -> undefined.
