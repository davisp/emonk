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
    etap:plan(9),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    etap:is(emonk:start(), ok, "Started emonk driver."),

    etap:fun_is(
        fun
            ({'EXIT', {badarg, _}}) -> true;
            (_) -> false
        end,
        (catch open_port({spawn_driver, "emonk_drv foobar"}, [binary])),
        "Opening a port with a bad settings string fails."
    ),

    test_no_settings(),
    test_valid_settings(),
    test_ignore_settings(),
    test_invalid_settings(),
    ok.

test_no_settings() ->
    {ok, Port} = emonk:new(),
    etap:is(is_port(Port), true, "Returned a valid port."),
    etap:is(emonk:destroy(Port), ok, "Stopped the port.").

test_valid_settings() ->
    {ok, Port} = emonk:new([{rt_max_bytes, 8388608}]),
    etap:is(is_port(Port), true, "Returned a valid port with settings."),
    etap:is(emonk:destroy(Port), ok, "Stopped the port.").

test_ignore_settings() ->
    {ok, Port} = emonk:new([foo]),
    etap:is(is_port(Port), true, "Returned a valid port ignoring settings."),
    etap:is(emonk:destroy(Port), ok, "Stopped the port.").

test_invalid_settings() ->
    etap:fun_is(
        fun
            ({'EXIT', {badarg, _}}) -> true;
            (_) -> false
        end,
        (catch emonk:new([{rt_max_bytes, -10}])),
        "Invalid settings cause an error."
    ).
