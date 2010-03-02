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
    etap:plan(29),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    {ok, Ctx} = emonk:new_context(),

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
    run_tests(Ctx, Tests).

run_tests(_, []) ->
    ok;
run_tests(Ctx, [E1 | Tests]) ->
    E2 = sort(E1),
    {ok, undefined} = emonk:eval(Ctx, js()),
    Msg = io_lib:format("Roundtrip: ~p", [E2]),
    {ok, Result} = emonk:call(Ctx, <<"test">>, [E2]),
    etap:is(sort(Result), [E2], lists:flatten(Msg)),
    run_tests(Ctx, Tests).

js() -> <<"var test = function(arg) {return [arg];};">>.

% Sort this shit out so that altered object property
% ordering doesn't make us evaluate inequal.
% Arrays are not altered, just recursed through to
% reach all objects.
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
