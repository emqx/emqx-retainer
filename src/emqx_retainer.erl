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

-module(emqx_retainer).

-behaviour(gen_server).

-include("emqx_retainer.hrl").

-include_lib("emqx/include/emqx.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%% gen_mod Callbacks
-export([load/1, unload/0]).

%% Hook Callbacks
-export([on_session_subscribed/4, on_message_publish/2]).

%% API Function Exports
-export([start_link/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {stats_fun, expiry_interval, stats_timer, expire_timer}).

%%--------------------------------------------------------------------
%% Load/Unload
%%--------------------------------------------------------------------

load(Env) ->
    emqx:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]).

on_session_subscribed(_ClientId, _Username, {Topic, _Opts}, _Env) ->
    SessPid = self(),
    Msgs = case emqx_topic:wildcard(Topic) of
               false -> read_messages(Topic);
               true  -> match_messages(Topic)
           end,
    lists:foreach(fun(Msg) -> SessPid ! {dispatch, Topic, Msg} end, sort_retained(Msgs)).

sort_retained([]) ->
    [];
sort_retained([Msg]) ->
    [Msg];
sort_retained(Msgs) ->
    lists:sort(fun(#message{timestamp = Ts1},
                   #message{timestamp = Ts2}) ->
                       Ts1 =< Ts2
               end, Msgs).

on_message_publish(Msg = #message{retain = false}, _Env) ->
    {ok, Msg};

%% RETAIN flag set to 1 and payload containing zero bytes
on_message_publish(Msg = #message{retain = true, topic = Topic, payload = <<>>}, _Env) ->
    mnesia:dirty_delete(retained, Topic),
    {ok, Msg};

on_message_publish(Msg = #message{retain = true, headers = Headers}, Env) ->
    case lists:member(retained, Headers) of
        true  -> {ok, Msg};
        false -> Msg1 = Msg#message{headers = lists:usort([retained|Headers)},
                 store_retained(Msg1, Env),
                 {ok, Msg1}
    end.

store_retained(Msg = #message{topic = Topic, payload = Payload, timestamp = Ts}, Env) ->
    case {is_table_full(Env), is_too_big(size(Payload), Env)} of
        {false, false} ->
            mnesia:dirty_write(#retained{topic = Topic, msg = Msg, ts = emqx_time:now_ms(Ts)}),
            emqx_metrics:set('messages/retained', retained_count());
        {true, _} ->
            lager:error("Cannot retain message(topic=~s) for table is full!", [Topic]);
        {_, true}->
            lager:error("Cannot retain message(topic=~s, payload_size=~p) "
                        "for payload is too big!", [Topic, byte_size(Payload)])
    end.

is_table_full(Env) ->
    Limit = proplists:get_value(max_message_num, Env, 0),
    Limit > 0 andalso (retained_count() > Limit).

is_too_big(Size, Env) ->
    Limit = proplists:get_value(max_payload_size, Env, 0),
    Limit > 0 andalso (Size > Limit).

unload() ->
    emqx:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Start the retainer
-spec(start_link(Env :: list()) -> {ok, pid()} | ignore | {error, any()}).
start_link(Env) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Env], []).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([Env]) ->
    Copies = case proplists:get_value(storage_type, Env, disc) of
                 ram       -> ram_copies;
                 disc      -> disc_copies;
                 disc_only -> disc_only_copies
             end,
    ok = ekka_mnesia:create_table(retained, [
                {type, set},
                {Copies, [node()]},
                {record_name, retained},
                {attributes, record_info(fields, retained)},
                {storage_properties, [{ets, [compressed]},
                                      {dets, [{auto_save, 1000}]}]}]),
    ok = ekka_mnesia:copy_table(retained),
    case mnesia:table_info(retained, storage_type) of
        Copies -> ok;
        _      -> {atomic, ok} = mnesia:change_table_copy_type(retained, node(), Copies)
    end,
    StatsFun = emqx_stats:statsfun('retained/count', 'retained/max'),
    {ok, StatsTimer}  = timer:send_interval(timer:seconds(1), stats),
    State = #state{stats_fun = StatsFun, stats_timer = StatsTimer},
    {ok, start_expire_timer(proplists:get_value(expiry_interval, Env, 0), State)}.

start_expire_timer(0, State) ->
    State;
start_expire_timer(undefined, State) ->
    State;
start_expire_timer(Ms, State) ->
    {ok, Timer} = timer:send_interval(Ms, expire),
    State#state{expiry_interval = Ms, expire_timer = Timer}.

handle_call(Req, _From, State) ->
    emqx_log:error("[Retainer] Uexpected request: ~p", [Req]),
    {reply, ignore, State}.

handle_cast(Msg, State) ->
    emqx_log:error("[Retainer] Uexpected msg: ~p", [Msg]),
    {noreply, State}.

handle_info(stats, State = #state{stats_fun = StatsFun}) ->
    StatsFun(retained_count()),
    {noreply, State, hibernate};

handle_info(expire, State = #state{expiry_interval = Never})
    when Never =:= 0 orelse Never =:= undefined ->
    {noreply, State, hibernate};

handle_info(expire, State = #state{expiry_interval = Interval}) ->
    expire_messages(emqx_time:now_ms() - Interval),
    {noreply, State, hibernate};

handle_info(Info, State) ->
    emqx_log:error("[Retainer] Uexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{stats_timer = TRef1, expire_timer = TRef2}) ->
    timer:cancel(TRef1), timer:cancel(TRef2).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec(read_messages(binary()) -> [message()]).
read_messages(Topic) ->
    [Msg || #retained{msg = Msg} <- mnesia:dirty_read(retained, Topic)].

-spec(match_messages(binary()) -> [message()]).
match_messages(Filter) ->
    %% TODO: optimize later...
    Fun = fun(#retained{topic = Name, msg = Msg}, Acc) ->
            case emqx_topic:match(Name, Filter) of
                true -> [Msg|Acc];
                false -> Acc
            end
          end,
    mnesia:async_dirty(fun mnesia:foldl/3, [Fun, [], retained]).

-spec(expire_messages(pos_integer()) -> any()).
expire_messages(Time) when is_integer(Time) ->
    mnesia:transaction(
        fun() ->
            Match = ets:fun2ms(
                        fun(#retained{topic = Topic, ts = Ts})
                            when Time > Ts -> Topic
                        end),
            Topics = mnesia:select(mqtt_retained, Match, write),
            lists:foreach(fun(<<"$SYS/", _/binary>>) -> ok; %% ignore $SYS/# messages
                             (Topic) -> mnesia:delete({mqtt_retained, Topic})
                           end, Topics)
        end).

-spec(retained_count() -> non_neg_integer()).
retained_count() -> mnesia:table_info(mqtt_retained, size).

