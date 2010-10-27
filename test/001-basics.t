#! /usr/bin/env escript
% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    Modules = [
        emonk
    ],

    etap:plan(length(Modules)),
    lists:foreach(fun(Mod) ->
        Mesg = atom_to_list(Mod) ++ " module loaded",
        etap:loaded_ok(Mod, Mesg)
    end, Modules),
    etap:end_tests().

