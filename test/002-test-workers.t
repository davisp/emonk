#! /usr/bin/env escript

test_adding(0, Count) ->
    test_no_more_room(10, Count),
    test_removing(emonk:num_workers()-1, emonk:num_workers());
test_adding(Num, Count) ->
    etap:is(emonk:num_workers(), Count, "Correct worker count."),
    etap:is(emonk:add_worker(), ok, "Added worker."),
    etap:is(emonk:num_workers(), Count+1, "Number of workers updated."),
    test_adding(Num-1, Count+1).

test_no_more_room(0, _) ->
    ok;
test_no_more_room(N, Count) ->
    etap:is(emonk:num_workers(), Count, "Correct worker count."),
    etap:is(emonk:add_worker(), error, "No more workers allowed."),
    test_no_more_room(N-1, Count).

test_removing(0, _) ->
    ok;
test_removing(Num, Count) ->
    etap:is(emonk:num_workers(), Count, "Correct worker count."),
    etap:is(emonk:rem_worker(), ok, "Removed worker ok."),
    etap:is(emonk:num_workers(), Count-1, "Number of workers updated."),
    test_removing(Num-1, Count-1).

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(5 + (63*6) + 20),
    etap:is(emonk:num_workers(), 1, "Initialized with 1 worker."),
    etap:is(emonk:add_worker(), ok, "Adding a worker succeeded."),
    etap:is(emonk:num_workers(), 2, "New worker changes worker count."),
    etap:is(emonk:rem_worker(), ok, "Removing a worker succeeded"),
    etap:is(emonk:num_workers(), 1, "Dead worker changes worker count."),
    test_adding(63, 1),
    
    etap:end_tests().

