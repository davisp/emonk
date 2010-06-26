#!/usr/bin/env escript

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    test_no_settings(),
    test_valid_settings(),
    test_ignore_settings(),
    test_invalid_settings(),
    ok.

test_no_settings() ->
    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        emonk:new_context(),
        "Returned a valid context with no options."
    ).

test_valid_settings() ->
    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        emonk:new_context([{rt_max_bytes, 1024*1024}]),
        "Returned a valid context with options specified."
    ).

test_ignore_settings() ->
    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        emonk:new_context(foo),
        "Ignores invalid option specifications."
    ).

test_invalid_settings() ->
    etap:fun_is(
        fun({error, _}) -> true; (_) -> false end,
        emonk:new_context([{rt_max_bytes, -10}]),
        "Invalid option values cause an error."
    ).
