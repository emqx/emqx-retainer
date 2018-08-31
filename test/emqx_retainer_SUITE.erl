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

-module(emqx_retainer_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-define(APP, emqx).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-import(lists, [nth/2]).

-define(TOPICS, [<<"TopicA">>, <<"TopicA/B">>, <<"Topic/C">>, <<"TopicA/C">>,
                 <<"/TopicA">>]).

-define(WILD_TOPICS, [<<"TopicA/+">>, <<"+/C">>, <<"#">>, <<"/#">>, <<"/+">>,
                      <<"+/+">>, <<"TopicA/#">>]).


-define(MQTT_SSL_TWOWAY, [{cacertfile, "certs/cacert.pem"},
                          {verify, verify_peer},
                          {fail_if_no_peer_cert, true}]).

-define(MQTT_SSL_CLIENT, [{keyfile, "certs/client-key.pem"},
                          {cacertfile, "certs/cacert.pem"},
                          {certfile, "certs/client-cert.pem"}]).

-define(CIPHERS,    [{ciphers,
                        ["ECDHE-ECDSA-AES256-GCM-SHA384",
                         "ECDHE-RSA-AES256-GCM-SHA384",
                         "ECDHE-ECDSA-AES256-SHA384",
                         "ECDHE-RSA-AES256-SHA384","ECDHE-ECDSA-DES-CBC3-SHA",
                         "ECDH-ECDSA-AES256-GCM-SHA384",
                         "ECDH-RSA-AES256-GCM-SHA384",
                         "ECDH-ECDSA-AES256-SHA384","ECDH-RSA-AES256-SHA384",
                         "DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
                         "AES256-GCM-SHA384","AES256-SHA256",
                         "ECDHE-ECDSA-AES128-GCM-SHA256",
                         "ECDHE-RSA-AES128-GCM-SHA256",
                         "ECDHE-ECDSA-AES128-SHA256",
                         "ECDHE-RSA-AES128-SHA256",
                         "ECDH-ECDSA-AES128-GCM-SHA256",
                         "ECDH-RSA-AES128-GCM-SHA256",
                         "ECDH-ECDSA-AES128-SHA256","ECDH-RSA-AES128-SHA256",
                         "DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
                         "AES128-GCM-SHA256","AES128-SHA256",
                         "ECDHE-ECDSA-AES256-SHA","ECDHE-RSA-AES256-SHA",
                         "DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
                         "ECDH-RSA-AES256-SHA","AES256-SHA",
                         "ECDHE-ECDSA-AES128-SHA","ECDHE-RSA-AES128-SHA",
                         "DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
                         "ECDH-RSA-AES128-SHA","AES128-SHA"]}]).

publish_retained_message(ClientId, Qos, Payload) ->
    emqx_client:publish(ClientId, nth(2, ?TOPICS), Payload, [{qos, Qos}, {retain, true}]),
    emqx_client:publish(ClientId, nth(3, ?TOPICS), Payload, [{qos, Qos}, {retain, true}]),
    emqx_client:publish(ClientId, nth(4, ?TOPICS), Payload, [{qos, Qos}, {retain, true}]).

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
    after 10 ->
            Msgs
    end.

all() -> [retained_message_test].

retained_message_test(_Config) ->
    ct:print("Retained message test starting"),

    %% Retained messages
    {ok, C1, _} = emqx_client:start_link([{clean_start, true}]),
    publish_retained_message(C1, 0, <<"Qos0">>),
    ok = emqx_client:disconnect(C1),
    timer:sleep(20),
    {ok, C2, _} = emqx_client:start_link([{clean_start, true}]),
    {ok, undefined, [0]} = emqx_client:subscribe(C2, nth(6, ?WILD_TOPICS), 0),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqx_client:disconnect(C2),

    {ok, C3, _} = emqx_client:start_link([{clean_start, true}]),
    publish_retained_message(C3, 1, <<"Qos1">>),
    ok = emqx_client:disconnect(C3),
    timer:sleep(20),
    {ok, C4, _} = emqx_client:start_link([{clean_start, true}]),
    {ok, undefined, [1]} = emqx_client:subscribe(C4, nth(6, ?WILD_TOPICS), 1),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqx_client:disconnect(C4),

    {ok, C5, _} = emqx_client:start_link([{clean_start, true}]),
    publish_retained_message(C5, 2, <<"Qos2">>),
    ok = emqx_client:disconnect(C5),
    timer:sleep(20),
    {ok, C6, _} = emqx_client:start_link([{clean_start, true}]),
    {ok, undefined, [2]} = emqx_client:subscribe(C6, nth(6, ?WILD_TOPICS), 2),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqx_client:disconnect(C6),

    {ok, C7, _} = emqx_client:start_link([{clean_start, true}]),
    publish_retained_message(C7, 2, <<"">>),
    ok = emqx_client:disconnect(C7),
    timer:sleep(20),
    {ok, C8, _} = emqx_client:start_link([{clean_start, true}]),
    {ok, undefined, [2]} = emqx_client:subscribe(C8, nth(6, ?WILD_TOPICS), 2),
    ?assertEqual(0, length(receive_messages(3))),
    ok = emqx_client:disconnect(C8).



init_per_suite(Config) ->
    [run_setup_steps(App) || App <- [emqx, emqx_management, emqx_retainer]],
    Config.

end_per_suite(_Config) ->
    run_teardown_steps().


run_setup_steps(App) ->
    NewConfig = generate_config(App),
    lists:foreach(fun set_app_env/1, NewConfig),
    application:ensure_all_started(App),
    ct:log("Applications: ~p", [application:loaded_applications()]).

run_teardown_steps() ->
    application:stop(emqx_retainer),
    emqx:shutdown().

generate_config(?APP) ->
    Schema = cuttlefish_schema:files([local_path(["deps", "emqx", "priv", "emqx.schema"])]),
    Conf = conf_parse:file([local_path(["deps", "emqx", "etc", "emqx.conf"])]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(emqx_management) ->
    Schema = cuttlefish_schema:files([local_path(["deps", "emqx_management", "priv", "emqx_management.schema"])]),
    Conf = conf_parse:file([local_path(["deps", "emqx_management", "etc", "emqx_management.conf"])]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(emqx_retainer) ->
    Schema = cuttlefish_schema:files([local_path(["priv", "emqx_retainer.schema"])]),
    Conf = conf_parse:file([local_path(["etc", "emqx_retainer.conf"])]),
    cuttlefish_generator:map(Schema, Conf).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

get_base_dir() ->
    get_base_dir(?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).

set_app_env({App, Lists}) ->
    lists:foreach(fun({acl_file, _Var}) ->
                      application:set_env(App, acl_file, local_path(["deps", "emqx", "etc", "acl.conf"]));
                     ({plugins_loaded_file, _Var}) ->
                      application:set_env(App, plugins_loaded_file, local_path(["deps","emqx", "test", "emqx_SUITE_data", "loaded_plugins"]));
                     ({Par, Var}) ->
                      application:set_env(App, Par, Var)
                  end, Lists).

change_opts(SslType) ->
    {ok, Listeners} = application:get_env(?APP, listeners),
    NewListeners =
    lists:foldl(fun({Protocol, Port, Opts} = Listener, Acc) ->
    case Protocol of
    ssl ->
            SslOpts = proplists:get_value(ssl_options, Opts),
            Keyfile = local_path(["etc/certs", "key.pem"]),
            Certfile = local_path(["etc/certs", "cert.pem"]),
            TupleList1 = lists:keyreplace(keyfile, 1, SslOpts, {keyfile, Keyfile}),
            TupleList2 = lists:keyreplace(certfile, 1, TupleList1, {certfile, Certfile}),
            TupleList3 =
            case SslType of
            ssl_twoway->
                CAfile = local_path(["etc", proplists:get_value(cacertfile, ?MQTT_SSL_TWOWAY)]),
                MutSslList = lists:keyreplace(cacertfile, 1, ?MQTT_SSL_TWOWAY, {cacertfile, CAfile}),
                lists:merge(TupleList2, MutSslList);
            _ ->
                lists:filter(fun ({cacertfile, _}) -> false;
                                 ({verify, _}) -> false;
                                 ({fail_if_no_peer_cert, _}) -> false;
                                 (_) -> true
                             end, TupleList2)
            end,
            [{Protocol, Port, lists:keyreplace(ssl_options, 1, Opts, {ssl_options, TupleList3})} | Acc];
        _ ->
            [Listener | Acc]
    end
    end, [], Listeners),
    application:set_env(?APP, listeners, NewListeners).

client_ssl_twoway() ->
    [{Key, local_path(["etc", File])} || {Key, File} <- ?MQTT_SSL_CLIENT] ++ ?CIPHERS.

client_ssl() ->
    ?CIPHERS ++ [{reuse_sessions, true}].
