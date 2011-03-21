#!/usr/bin/env escript
%%! -pa ./test/ -pa ./ebin/
% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.

main(_) ->
    test_util:run(unknown, fun() -> test() end).

test() ->
    {ok, Ctx} = emonk:create_ctx(),
    test_send_exists(Ctx),
    test_send_message(Ctx),
    ok.

test_send_exists(Ctx) ->
    JS = <<"var f = (erlang.send !== undefined); f;">>,
    etap:is(emonk:eval(Ctx, JS), {ok, true}, "erlang.send function exists.").

test_send_message(Ctx) ->
    JS = <<"var f = erlang.send(1.3) == 2.6; f;">>,
    {message, Token, Data} = emonk:eval(Ctx, JS),
    etap:is(emonk:send(Ctx, Token, Data * 2), {ok, true}, "message passed ok").
