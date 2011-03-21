#! /usr/bin/env escript
%%! -pa ./test/ -pa ./ebin/
% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.


main([]) ->
    test_util:run(100, fun() -> test() end).

test() ->
    lists:foreach(fun(_) -> test_vm_creation() end, lists:seq(1, 100, 1)),
    erlang:garbage_collect().

test_vm_creation() ->
    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        emonk:create_ctx(),
        "Created a context successfully."
    ),
    erlang:garbage_collect().

