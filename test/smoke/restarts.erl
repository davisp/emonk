-module(restarts).

-export([run/0]).

run(0) ->
    ok;
run(N) ->
    {ok, Ctx} = emonk:create_ctx(),
    emonk:eval(Ctx, <<"var f = function(x) {return x*3;};">>),
    {ok, 27} = emonk:call(Ctx, <<"f">>, [9]),
    run(N-1).

run() ->
    io:format(".", []),
    code:add_path("ebin"),
    run(5),
    init:restart().

