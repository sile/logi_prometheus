%% @copyright 2018 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_prometheus_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init([]) ->
    Counter = #{
      id      => logi_prometheus_counter,
      start   => {logi_prometheus_counter, start_link, []},
      restart => permanent
     },
    {ok, {#{}, [Counter]} }.
