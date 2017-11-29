%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emq_retainer_cli).

-include("emq_retainer.hrl").

-include_lib("emqttd/include/emqttd_cli.hrl").

-export([load/0, cmd/1, unload/0]).

load() -> emqttd_ctl:register_cmd(retainer, {?MODULE, cmd}, []).

cmd(["info"]) ->
    ?PRINT("retained/total: ~w~n", [mnesia:table_info(mqtt_retained, size)]);

cmd(["topics"]) ->
    case mnesia:dirty_all_keys(mqtt_retained) of
        [] -> ignore;
        Ts -> ?PRINT("~s~n", [lists:join("\n", Ts)])
    end;

cmd(["clean"]) ->
    Size = mnesia:table_info(mqtt_retained, size),
    case mnesia:clear_table(mqtt_retained) of
        {atomic, ok} -> ?PRINT("Cleaned ~p retained messages~n", [Size]);
        {aborted, R} -> ?PRINT("Aborted ~p~n", [R])
    end;

cmd(_) ->
    ?USAGE([{"retainer info",   "Show the count of retained messages"},
            {"retainer topics", "Show all topics of retained messages"},
            {"retainer clean",  "Clean all retained messages"}]).

unload() -> emqttd_ctl:unregister_cmd(retainer).

