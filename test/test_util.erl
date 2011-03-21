% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.
-module(test_util).

-export([run/2]).

run(Plan, Fun) ->
    etap:plan(Plan),
    try
        Fun(),
        etap:end_tests()
    catch
        Type:Error ->
            etap:diag("Test died abnormally: ~p:~p", [Type, Error]),
            etap:diag("~p", [erlang:get_stacktrace()]),
            etap:bail()
    end.

