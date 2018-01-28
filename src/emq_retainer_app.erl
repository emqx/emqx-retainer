%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_retainer_app).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Env = application:get_all_env(emq_retainer),
    {ok, Sup} = emq_retainer_sup:start_link(Env),
    emq_retainer:load(Env),
    emq_retainer_config:register(),
    emq_retainer_cli:load(),
    {ok, Sup}.

stop(_State) ->
    emq_retainer_cli:unload(),
    emq_retainer:unload(),
    emq_retainer_config:unregister().

