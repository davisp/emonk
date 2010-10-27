#! /usr/bin/env escript
% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.

main([]) ->
    run().

do_context(Script) ->
    {ok, Ctx} = emonk:create_ctx(),
    emonk:eval(Ctx, <<"var process={};var f = ", Script/binary, " f(process);">>),
    {ok, Resp} = emonk:eval(Ctx, <<"process.add_message('test');process.next_message();">>),
    <<"test">> = Resp.

create_context(0, _Script) ->
    ok;
create_context(N, Script) ->
    do_context(Script),
    io:format("Num: ~p\n", [N]),
    create_context(N-1, Script).

run() ->
    code:add_path("ebin"),
    Script = <<"
    (function(process) {
    
        var processQueue = []; 

        process.next_message = function() {
            return processQueue.pop()
        }
    
        process.add_message = function(message) {
            processQueue.push(message);
        }

        function log(message) {
            process.add_message(['log', message])
        }

        log('test');

    });
    ">>,
    create_context(100000, Script).
