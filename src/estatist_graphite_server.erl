-module(estatist_graphite_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    sending_timer_ref,
    sending_interval,
    host,
    port,
    prefix
    % metrics
}).

%% опции:
%%  - интервал отсылки
%%  - префикс для метрик
%%  - хост и порт
%%  - список метрик для отправки (пока сделаем отправку всего)

start_link() ->
    start_link([]).
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

init(Options) ->
    S1 = parse_options(Options, #state{}),
    S2 = schedule_sending(S1),
    {ok, S2}.

handle_call(Call, _From, S) ->
    exit({'unknown call', Call}),
    {reply, ok, S}.

handle_cast(Cast, S) ->
    exit({'unknown cast', Cast}),
    {noreply, S}.

handle_info({timeout, _, sending_timeout}, S) ->
    send_statistics(S),
    {noreply, schedule_sending(S)};
handle_info(Info, S) ->
    exit({'unknown info', Info}),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%% local
parse_options(Options, S) ->
    S#state{
        sending_interval = proplists:get_value(sending_interval, Options, 60         ),
        host             = proplists:get_value(host            , Options, "localhost"),
        port             = proplists:get_value(port            , Options, 2003       ),
        prefix           = proplists:get_value(prefix          , Options, [estatist] )
        % metrics          = proplists:get_value(metrics         , Options, []         )
    }.

%% sending timer
schedule_sending(S) ->
    cancel_sending(S),
    S#state{sending_timer_ref=erlang:start_timer(S#state.sending_interval * 1000, self(), sending_timeout)}.

cancel_sending(S=#state{sending_timer_ref=undefined}) ->
    S;
cancel_sending(S=#state{}) ->
    erlang:cancel_timer(S#state.sending_timer_ref),
    S#state{sending_timer_ref=undefined}.

%% sending logic
send_statistics(S) ->
    {ok, Stats} = estatist:select_all(),
    send_to_graphite(prepare_stats(S#state.prefix, Stats, timestamp()), S).

prepare_stats(Prefix, Stats, Ts) when is_list(Stats) ->
    lists:foldl(
        fun(Stat, Acc) ->
            Acc ++ prepare_stats(Prefix, Stat, Ts)
        end, [], Stats
    );
prepare_stats(Prefix, {Key, Subtree}, Ts) ->
    lists:foldl(
        fun(Stat, Acc) ->
            prepare_stats_iter(Prefix ++ [Key], Stat, Acc, Ts)
        end, [], Subtree
    ).

prepare_stats_iter(Prefix, {Key, SubTree}, Acc, Ts) when is_list(SubTree) ->
    Acc ++ prepare_stats(Prefix, {Key, SubTree}, Ts);
prepare_stats_iter(Prefix, {Key, Value}, Acc, Ts) ->
    Acc ++ make_graphite_metric_line(Prefix++[Key], Value, Ts).


%% graphite utils
make_graphite_metric_line(Path, Value, Ts) ->
    [make_graphite_path(Path), " ", to_list(Value), " ", to_list(Ts), "\n"].

make_graphite_path(Path) ->
    join_iolist(".", lists:map(fun to_list/1, Path)).

send_to_graphite(Msg, S) ->
    % exit(iolist_to_binary(Msg)),
    % io:format("sending ~s~n", [iolist_to_binary(Msg)]),
    case gen_tcp:connect(S#state.host, S#state.port, [list, {packet, 0}]) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, Msg),
            ok = gen_tcp:close(Sock);
        {error, Error} ->
            error_logger:warning_msg("failed to send estatist metrics to graphite ~p", [Error])
    end.


to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

join_iolist(_    , []    ) -> [];
join_iolist(_    , [H|[]]) -> H;
join_iolist(Delim, [H|T] ) -> [H, Delim, join_iolist(Delim, T)].

timestamp() ->
    {Mega, Secs, _} = erlang:now(),
    Mega * 1000000 + Secs.
