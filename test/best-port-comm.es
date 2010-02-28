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
    io:format("Starting driver.~n"),
    ok = emonk:start(),
    io:format("Getting port.~n"),
    {ok, Port} = emonk:new(),

    io:format("Evaluating script.~n"),
    Script = <<"var echo = function(arg) {return [arg];};">>,
    {ok, _} = emonk:eval(Port, Script),

    io:format("Bulding payload.~n"),
    Term = data(1024*64, []),

    io:format("Running test.~n"),
    timeit(fun() -> flood(Port, Term, 1000) end).

flood(_, _, 0) ->
    ok;
flood(Port, Term, Count) ->
    {ok, _} = emonk:call(Port, <<"echo">>, [Term]),
    flood(Port, Term, Count-1).

timeit(Func) ->
    Before = erlang:now(),
    Func(),
    Diff = timer:now_diff(now(), Before),
    io:format("Time: ~p~n", [Diff]).

data(Size, Acc) ->
    AccSize = size(term_to_binary(Acc)),
    Row = {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 2.13}]},
    NumIters = (Size-AccSize) div size(term_to_binary(Row)),
    case {AccSize, NumIters} of
        _ when AccSize > Size ->
            Acc;
        _ ->
            Acc2 = quick_data(NumIters+1, Row, Acc),
            data(Size, Acc2)
    end.

quick_data(0, _, Acc) ->
    Acc;
quick_data(Count, Term, Acc) ->
    quick_data(Count-1, Term, [Term | Acc]).
