%% @copyright 2018 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_prometheus_app).

-behaviour(application).

%%------------------------------------------------------------------------------
%% 'application' Callback API
%%------------------------------------------------------------------------------
-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% 'application' Callback Functions
%%------------------------------------------------------------------------------
%% @private
start(_StartType, _StartArgs) ->
    logi_prometheus_sup:start_link().

%% @private
stop(_State) ->
    ok.
