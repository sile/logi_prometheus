%% @copyright 2018 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_prometheus_counter).

-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).
-export([declare/1]).
-export([increment/4]).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% Types & Macros & Records
%%------------------------------------------------------------------------------
-type registry() :: atom() | string().
-type table_key() :: {registry(), logi:logger_id(), logi:severity(), atom(), module()}.

-define(TABLE, ?MODULE).

-define(STATE, ?MODULE).
-record(?STATE,
        {
         delayed = #{} :: #{table_key() => pos_integer()}
        }).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec declare(registry()) -> ok.
declare(Registry) ->
    prometheus_counter:declare(
      [
       {name, logi_messages_total},
       {help, "Messages count"},
       {labels, [logger, severity, application, module]},
       {registry, Registry}
      ]),
    ok.

-spec increment(Registry, LoggerId, Context, InitialDelay) -> ok when
      Registry :: registry(),
      LoggerId :: logi:logger_id(),
      Context :: logi_context:context(),
      InitialDelay :: logi_prometheus_sink:non_neg_milliseconds().
increment(Registry, LoggerId, Context, InitialDelay) ->
    Location = logi_context:get_location(Context),
    Severity = logi_context:get_severity(Context),
    App = logi_location:get_application(Location),
    Module = logi_location:get_module(Location),
    Labels = [LoggerId, Severity, App, Module],
    Key = {Registry, LoggerId, Severity, App, Module},
    case InitialDelay =:= 0 orelse ets:member(?TABLE, Key) of
        true  -> prometheus_counter:inc(Registry, logi_messages_total, Labels, 1);
        false ->
            prometheus_counter:inc(Registry, logi_messages_total, Labels, 0),
            gen_server:cast(?MODULE, {delayed_increment, {Key, InitialDelay}})
    end,
    ok.

%%------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init([]) ->
    _ = ets:new(?TABLE, [named_table, protected, {read_concurrency, true}]),
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({delayed_increment, Arg}, State0) ->
    State1 = handle_delayed_increment(Arg, State0),
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({force_increment, Arg}, State0) ->
    State1 = handle_force_incremenet(Arg, State0),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec handle_delayed_increment(Arg, #?STATE{}) -> #?STATE{} when
      Arg :: {table_key(), logi_prometheus_sink:non_neg_milliseconds()}.
handle_delayed_increment({Key, Delay0}, State) ->
    Delay1 =
        case ets:member(?TABLE, Key) of
            true  -> 0;
            false -> Delay0
        end,
    N = maps:get(Key, State#?STATE.delayed, 0),
    Map = maps:put(Key, N + 1, State#?STATE.delayed),
    _ = case N of
            0 -> erlang:send_after(Delay1, self(), {force_increment, Key});
            _ -> ok
        end,
    State#?STATE{delayed = Map}.

-spec handle_force_incremenet(table_key(), #?STATE{}) -> #?STATE{}.
handle_force_incremenet(Key, State) ->
    ets:insert(?TABLE, {Key, true}),

    {Registry, LoggerId, Severity, App, Module} = Key,
    Labels = [LoggerId, Severity, App, Module],
    Count = maps:get(Key, State#?STATE.delayed),
    prometheus_counter:inc(Registry, logi_messages_total, Labels, Count),

    Map = maps:remove(Key, State#?STATE.delayed),
    State#?STATE{delayed = Map}.
