#!/usr/bin/env escript
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

main(_) ->
    etap:plan(1),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    true = emonk_driver:start(),
    {ok, Port} = emonk_driver:new(),

    test_response(Port),
    test_undefined(Port),
    test_error(Port),
    ok.

test_response(Port) ->
    etap:is(
        emonk_driver:call_driver(Port, <<"var x = 2; x*3;">>),
        {ok, 6},
        "Successful roundtrip through the JS vm."
    ).

test_undefined(Port) ->
    etap:is(
        emonk_driver:call_driver(Port, <<"var x = function() {};">>),
        {ok, undefined},
        "Successfully ignored non-JSON response."
    ).

test_error(Port) ->
    etap:fun_is(
        fun
            ({error, {_, _, _}}) -> true;
            (_) -> false
        end,
        emonk_driver:call_driver(Port, <<"f * 3">>),
        "Reported the undefined error."
    ),
    
    etap:fun_is(
        fun
            ({error, {_, _, _}}) -> true;
            (_) -> false
        end,
        emonk_driver:call_driver(Port, <<"throw \"foo\";">>),
        "Reported the thrown exception."
    ).
