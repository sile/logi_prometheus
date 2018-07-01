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
-export([install/1, install/2, install_opt/2]).
-export([uninstall/0, uninstall/1]).

-export_type([non_neg_milliseconds/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type non_neg_milliseconds() :: non_neg_integer().
%% 非負のミリ秒

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
    install_opt(ConditionSpec, [{logger, Logger}]).

%% @doc オプション指定付きで、メトリクス収集用バックエンドをロガーに登録する
%%
%% === オプション ===
%%
%% `logger'は対象となるロガー。
%% デフォルト値は`logi:default_logger()'。
%%
%% `initial_delay'は、始めて使われたメトリクス(カウンター)の値をインクリメントするまでの遅延。
%% この値を、Prometheusのメトリクス収集間隔よりの長く設定しておけば、
%% (例えば)サーバの起動後、初めて発行されたエラーログも、Prometheusの`rate'関数等を用いて検知可能になる。
%% デフォルトは値は`0'.
-spec install_opt(logi_condition:spec(), Options) -> ok when
      Options :: [Option],
      Option :: {logger, logi:logger()}
              | {initial_delay, non_neg_milliseconds()}.
install_opt(ConditionSpec, Options) ->
    logi_prometheus_counter:declare(default),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    InitialDelay = proplists:get_value(initial_delay, Options, 0),
    BackendSpec = {?MODULE, undefined, ?MODULE, {Logger, InitialDelay}},
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
    {Logger, InitialDelay} = logi_backend:get_data(Backend),
    Key = {
           default,
           Logger,
           logi_msg_info:get_severity(Info),
           logi_location:get_application(Location),
           logi_location:get_module(Location)
          },
    logi_prometheus_counter:increment(Key, InitialDelay).
