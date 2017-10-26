%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A `logi' backend for collecting prometheus metrics.
%%
%% This exposes metrics that count the number of log messages sent to a channel.
%%
%% === EXAMPLE ===
%%
%% ```
%% %% Installs `logi_prometheus_backend' to the default channel.
%% ok = logi_prometheus_backend:install(info).
%%
%% %% Logs a message.
%% > logi:info("foo").
%%
%% %% Prints metrics.
%% > io:format(prometheus_text_format:format()).
%% # TYPE logi_messages_total counter
%% # HELP logi_messages_total Log messages count
%% logi_messages_total{logger="logi_default_logger",severity="info",application="stdlib",module="erl_eval"} 1
%% '''
-module(logi_prometheus_backend).

-behaviour(logi_backend).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install(logi:default_logger(), ConditionSpec)
-spec install(logi_condition:spec()) -> ok.
install(ConditionSpec) ->
    install(logi:default_logger(), ConditionSpec).

%% @doc Prometheus用のメトリクス収集バックエンドをLoggerに登録する
%%
%% 既に登録の場合は、内容が更新される
-spec install(logi:logger(), logi_condition:spec()) -> ok.
install(Logger, ConditionSpec) ->
    prometheus_counter:declare(
      [
       {name, logi_messages_total},
       {help, "Log messages count"},
       {labels, [logger, severity, application, module]}
      ]),
    BackendSpec = {?MODULE, undefined, ?MODULE, Logger},
    logi:set_backend(Logger, BackendSpec, ConditionSpec).

%% @equiv uninstall(logi:default_logger())
-spec uninstall() -> ok.
uninstall() ->
    uninstall(logi:default_logger()).

%% @doc バックエンドの登録を解除する
%%
%% バックエンドが未登録の場合は、エラーとはならずに単に無視される
-spec uninstall(logi:logger()) -> ok.
uninstall(Logger) ->
    logi:delete_backend(Logger, ?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Backend, Location, Info, _Format, _Data) ->
    Logger = logi_backend:get_data(Backend),
    Labels =
        [
         Logger,
         logi_msg_info:get_severity(Info),
         logi_location:get_application(Location),
         logi_location:get_module(Location)
        ],
    prometheus_counter:inc(logi_messages_total, Labels, 1).
