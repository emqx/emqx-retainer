%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_retainer_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-define(APP, emqx).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-import(lists, [nth/2]).

-define(TOPICS, [ <<"TopicA">>
                , <<"TopicA/B">>
                , <<"Topic/C">>
                , <<"TopicA/C">>
                , <<"/TopicA">>]).

-define(WILD_TOPICS, [ <<"TopicA/+">>
                     , <<"+/C">>
                     , <<"#">>
                     , <<"/#">>
                     , <<"/+">>
                     , <<"+/+">>
                     , <<"TopicA/#">>]).

all() -> [ test_message_expiry
         , test_expiry_timer
         , test_subscribe_topics
         ].

receive_messages(Count) ->
    receive_messages(Count, []).
receive_messages(0, Msgs) ->
    Msgs;
receive_messages(Count, Msgs) ->
    receive
        {publish, Msg} ->
            ct:log("Msg: ~p ~n", [Msg]),
            receive_messages(Count-1, [Msg|Msgs]);
        Other ->
            ct:log("Other Msg: ~p~n",[Other]),
            receive_messages(Count, Msgs)
    after 2000 ->
            Msgs
    end.

test_message_expiry(_) ->
    {ok, C1} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C1),
    emqtt:publish(C1, <<"qos/0">>, #{'Message-Expiry-Interval' => 2}, <<"QoS0">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C1, <<"qos/1">>, #{'Message-Expiry-Interval' => 2}, <<"QoS1">>, [{qos, 1}, {retain, true}]),
    emqtt:publish(C1, <<"qos/2">>, #{'Message-Expiry-Interval' => 2}, <<"QoS2">>, [{qos, 2}, {retain, true}]),
    timer:sleep(100),
    ok = emqtt:disconnect(C1),

    {ok, C2} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C2),
    {ok, #{}, [2]} = emqtt:subscribe(C2, <<"qos/+">>, 2),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqtt:disconnect(C2),

    %% Expire
    timer:sleep(3000),
    {ok, C3} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C3),
    {ok, #{}, [0]} = emqtt:subscribe(C3, <<"qos/+">>, 0),
    ?assertEqual(0, length(receive_messages(1))),
    ok = emqtt:disconnect(C3),

    {ok, C4} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C4),
    emqtt:publish(C4, <<"test/A">>, #{'Message-Expiry-Interval' => 0}, <<"don't expire">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C4, <<"test/B">>, #{'Message-Expiry-Interval' => 2}, <<"expire">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C4, <<"test/C">>, #{'Message-Expiry-Interval' => 5}, <<"don't expire">>, [{qos, 0}, {retain, true}]),
    % emqtt:publish(C4, <<"test/D">>, <<"don't expire if retainer.expiry_interval equals to 0">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C4, <<"$SYS/E">>, <<"don't expire">>, [{qos, 0}, {retain, true}]),
    timer:sleep(20),
    ok = emqtt:disconnect(C4),
    timer:sleep(3000),
    
    {ok, C5} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C5),
    {ok, #{}, [0]} = emqtt:subscribe(C5, <<"test/C">>, 0),
    ?assertEqual(1, length(receive_messages(1))),
    {ok, #{}, [0]} = emqtt:subscribe(C5, <<"test/+">>, 0),
    {ok, #{}, [0]} = emqtt:subscribe(C5, <<"$SYS/E">>, 0),
    ?assertEqual(3, length(receive_messages(4))),
    ok = emqtt:disconnect(C5).

%% expired message will be deleted by check timer
test_expiry_timer(_) ->
    {ok, C1} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C1),
    emqtt:publish(C1, <<"test/A">>, #{'Message-Expiry-Interval' => 0}, <<"don't expire">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C1, <<"test/B">>, #{'Message-Expiry-Interval' => 1}, <<"expire">>, [{qos, 0}, {retain, true}]),
    emqtt:publish(C1, <<"test/C">>, <<"expire">>, [{qos, 0}, {retain, true}]),
    ok = emqtt:disconnect(C1),

    timer:sleep(4000),
    {ok, C2} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C2),
    {ok, #{}, [0]} = emqtt:subscribe(C2, <<"test/+">>, 0),
    ok = emqtt:disconnect(C2).

test_subscribe_topics(_) ->
    {ok, C1} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C1),
    lists:foreach(fun(N) ->
                          emqtt:publish(C1, nth(N, ?TOPICS), #{'Message-Expiry-Interval' => 0}, <<"don't expire">>, [{qos, 0}, {retain, true}])
                  end, [1,2,3,4,5]),
    timer:sleep(20),
    ok = emqtt:disconnect(C1),
    timer:sleep(10),
    {ok, C2} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C2),
    {ok, #{}, [0]} = emqtt:subscribe(C2, nth(1, ?WILD_TOPICS), 0),
    ?assertEqual(2, length(receive_messages(2))),
    ok = emqtt:disconnect(C2),

    {ok, C3} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C3),
    {ok, #{}, [0]} = emqtt:subscribe(C3, nth(2, ?WILD_TOPICS), 0),
    ?assertEqual(2, length(receive_messages(2))),
    ok = emqtt:disconnect(C3),

    {ok, C4} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C4),
    {ok, #{}, [0]} = emqtt:subscribe(C4, nth(5, ?WILD_TOPICS), 0),
    ?assertEqual(1, length(receive_messages(1))),
    ok = emqtt:disconnect(C4),

    {ok, C5} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C5),
    {ok, #{}, [0]} = emqtt:subscribe(C5, nth(6, ?WILD_TOPICS), 0),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqtt:disconnect(C5).

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_retainer]),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx_retainer, emqx]).

init_per_testcase(TestCase, Config) ->
    case TestCase of
        test_message_expiry ->
            application:set_env(emqx_retainer, expiry_interval, 0),
            application:set_env(emqx_retainer, expiry_timer_interval, 0);
        test_expiry_timer ->
            application:set_env(emqx_retainer, expiry_interval, 2000),
            application:set_env(emqx_retainer, expiry_timer_interval, 1000);    % 1000ms
        test_subscribe_topics ->
            application:set_env(emqx_retainer, expiry_interval, 0),
            application:set_env(emqx_retainer, expiry_timer_interval, 0)
    end,
    application:ensure_all_started(emqx_retainer),
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(emqx_retainer).
