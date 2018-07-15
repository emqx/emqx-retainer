%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_retainer_cli).

-include("emqx_retainer.hrl").

-export([load/0, cmd/1, unload/0]).

load() ->
    emqx_ctl:register_command(retainer, {?MODULE, cmd}, []).

cmd(["info"]) ->
    emqx_cli:print("retained/total: ~w~n", [mnesia:table_info(retained, size)]);

cmd(["topics"]) ->
    case mnesia:dirty_all_keys(retained) of
        []     -> ignore;
        Topics -> lists:foreach(fun(Topic) -> emqx_cli:print("~s~n", [Topic]) end, Topics)
    end;

cmd(["clean"]) ->
    Size = mnesia:table_info(retained, size),
    case mnesia:clear_table(retained) of
        {atomic, ok} -> emqx_cli:print("Cleaned ~p retained messages~n", [Size]);
        {aborted, R} -> emqx_cli:print("Aborted ~p~n", [R])
    end;

cmd(_) ->
    emqx_cli:usage([{"retainer info",   "Show the count of retained messages"},
                    {"retainer topics", "Show all topics of retained messages"},
                    {"retainer clean",  "Clean all retained messages"}]).

unload() ->
    emqx_ctl:unregister_command(retainer).

