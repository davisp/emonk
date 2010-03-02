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
    etap:plan(unknown),
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
        {<<"null">>, null},
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"1">>, 1},
        {<<"-1">>, -1},
        {<<"3.1416">>, 3.1416},
        {<<"-3.1416">>, -3.1416},
        {<<"12.0e10">>, 12.0e10},
        {<<"1.234E+10">>, 1.234E+10},
        {<<"-1.234E-10">>, -1.234E-10},
        {<<"10.0">>, 10.0},
        {<<"123.456">>, 123.456},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"\"foo\\u0005bar\"">>, <<"foo", 5, "bar">>},
        {<<"\"\"">>, <<"">>},
        {<<"\"\\n\\n\\n\"">>, <<"\n\n\n">>},
        {<<"\"\\\" \\b\\f\\r\\n\\t\\\"\"">>, <<"\" \b\f\r\n\t\"">>},
        {<<"{}">>, {[]}},
        {<<"{\"foo\": \"bar\"}">>, {[{<<"foo">>, <<"bar">>}]}},
        {
            <<"{\"foo\": \"bar\", \"baz\": 123}">>,
            {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]}
        },
        {<<"[]">>, []},
        {<<"[[]]">>, [[]]},
        {<<"[1, \"foo\"]">>, [1, <<"foo">>]},
        {<<"{\"foo\": [123]}">>, {[{<<"foo">>, [123]}]}},
        {<<"{\"foo\": [1, 2, 3]}">>, {[{<<"foo">>, [1, 2, 3]}]}},
        {
            <<"{\"foo\": {\"bar\": true}}">>,
            {[{<<"foo">>, {[{<<"bar">>, true}]}}]}
        },
        {
            <<"{\"foo\": [], \"bar\": {\"baz\": true}, \"alice\": \"bob\"}">>,
            {[
                {<<"foo">>, []},
                {<<"bar">>, {[{<<"baz">>, true}]}}, {<<"alice">>, <<"bob">>}
            ]}
        },
        {
            <<"[-123, \"foo\", {\"bar\": []}, null]">>,
            [-123, <<"foo">>, {[{<<"bar">>, []}]}, null]            
        },
        {
            <<"new Date(\"2003-03-01T17:13:34.001Z\")">>,
            <<"2003-03-01T17:13:34.001Z">>
        }
    ],
    ok = run_tests(Ctx, Tests),
    test_converter(Ctx).

run_tests(_, []) ->
    ok;
run_tests(Ctx, [{Arg, Exp} | Tests]) ->
    {ok, Result} = emonk:eval(Ctx, js(Arg)),
    Msg = io_lib:format("~p", [Arg]),
    etap:is(sort(Result), [sort(Exp)], lists:flatten(Msg)),
    run_tests(Ctx, Tests).

test_converter(Ctx) ->
    Smoke = <<"Date.prototype.toJSON = function() {return {\"foo\": 2.1};};">>,
    Date = <<"new Date(\"2003-03-01T17:13:34.001Z\")">>,
    {ok, undefined} = emonk:eval(Ctx, Smoke),
    {ok, Result} = emonk:eval(Ctx, js(Date)),
    etap:is(sort(Result), [{[{<<"foo">>, 2.1}]}], "Is toJSON called?"),
    ok.

js(Arg) -> <<"(function(arg) {return [arg];})(", Arg/binary, ");">>.

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
