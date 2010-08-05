#! /usr/bin/env escript

main([]) ->
    start(8);
main([N]) ->
    start(list_to_integer(N)).

start(N) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    emonk:add_worker(),
    MonPid = spawn(fun() -> monitor([]) end),
    run(MonPid, N).

monitor(Pids) ->
    io:format("Monitoring ~p pids: ~p~n", [length(Pids), now()]),
    Pids2 = receive
        Pid ->
            [{Pid, now()} | proplists:delete(Pid, Pids)]
        after 1000 ->
            Pids
    end,
    check_pids(Pids2),
    monitor(Pids2).

check_pids([]) ->
    ok;
check_pids([{Pid, Last} | Rest]) ->
    case timer:now_diff(now(), Last) of
        Diff when Diff >= 5000000 ->
            io:format("Lost track of pid: ~p ~p~n", [Pid, Diff]);
        _ ->
            ok
    end,
    check_pids(Rest).

run(MonPid, N) ->
    {ok, Ctx} = emonk:create_ctx(),
    {ok, undefined} = emonk:eval(Ctx, js()),
    run(MonPid, Ctx, N).

run(_, _, 0) ->
    timer:sleep(1000),
    run(nil, 0);
run(MonPid, Ctx, N) ->
    Pid = spawn(fun() -> do_js(MonPid, Ctx, 250) end),
    MonPid ! Pid,
    run(MonPid, N-1).

do_js(MonPid, Ctx, 0) ->
    MonPid ! self(),
    do_js(MonPid, Ctx, 250);
do_js(MonPid, Ctx, Num) ->
    Test = random_test(),
    {ok, [Resp]} = emonk:call(Ctx, <<"f">>, [Test]),
    Sorted = sort(Resp),
    true = Test == Sorted,
    do_js(MonPid, Ctx, Num-1).

js() -> 
    <<"var f = function(x) {return [x];};">>.

random_test() ->
    Tests = [
        null,
        true,
        false,
        1,
        -1,
        3.1416,
        -3.1416,
        12.0e10,
        1.234E+10,
        -1.234E-10,
        10.0,
        123.456,
        10.0,
        <<"foo">>,
        <<"foo", 5, "bar">>,
        <<"">>,
        <<"\n\n\n">>,
        <<"\" \b\f\r\n\t\"">>,
        {[]},
        {[{<<"foo">>, <<"bar">>}]},
        {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
        [],
        [[]],
        [1, <<"foo">>],
        {[{<<"foo">>, [123]}]},
        {[{<<"foo">>, [1, 2, 3]}]},
        {[{<<"foo">>, {[{<<"bar">>, true}]}}]},
        {[
            {<<"foo">>, []},
            {<<"bar">>, {[{<<"baz">>, true}]}}, {<<"alice">>, <<"bob">>}
        ]},
        [-123, <<"foo">>, {[{<<"bar">>, []}]}, null]
    ],
    {_, [Test | _]} = lists:split(random:uniform(length(Tests)) - 1, Tests),
    sort(Test).

sort({Props}) ->
    objsort(Props, []);
sort(List) when is_list(List) ->
    lstsort(List, []);
sort(Other) ->
    Other.

objsort([], Acc) ->
    {lists:sort(Acc)};
objsort([{K,V} | Rest], Acc) ->
    objsort(Rest, [{K, sort(V)} | Acc]).

lstsort([], Acc) ->
    lists:reverse(Acc);
lstsort([Val | Rest], Acc) ->
    lstsort(Rest, [sort(Val) | Acc]).
