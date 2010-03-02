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
