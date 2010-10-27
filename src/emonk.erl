% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.

-module(emonk).
-on_load(init/0).


-export([create_ctx/0, create_ctx/1]).
-export([eval/2, eval/3, call/3, call/4, send/3, send/4]).


-define(APPNAME, emonk).
-define(LIBNAME, emonk).
-define(CTX_STACK, 8192).
-define(TIMEOUT, infinity).

create_ctx() ->
    create_ctx(?CTX_STACK).

create_ctx(_) ->
    not_loaded(?LINE).


eval(Ctx, Script) ->
    eval(Ctx, Script, ?TIMEOUT).

eval(Ctx, Script, Timeout) ->
    Ref = make_ref(),
    ok = eval(Ctx, Ref, self(), Script),
    receive
        {Ref, Resp} ->
            Resp;
        {message, Resp} ->
            {message, Ref, Resp};
        Other ->
            throw(Other)
    after Timeout ->
        throw({error, timeout, Ref})
    end.

eval(_Ctx, _Ref, _Dest, _Script) ->
    not_loaded(?LINE).


call(Ctx, Name, Args) ->
    call(Ctx, Name, Args, ?TIMEOUT).

call(Ctx, Name, Args, Timeout) ->
    Ref = make_ref(),
    ok = call(Ctx, Ref, self(), Name, Args),
    receive
        {Ref, Resp} ->
            Resp;
        {message, Resp} ->
            {message, Ref, Resp}
    after Timeout ->
        throw({error, timeout, Ref})
    end.

call(_Ctx, _Ref, _Dest, _Name, _Args) ->
    not_loaded(?LINE).

send(_Ctx, _Data) ->
    not_loaded(?LINE).

send(Ctx, Ref, Data) ->
    send(Ctx, Ref, Data, ?TIMEOUT).

send(Ctx, Ref, Data, Timeout) ->
    ok = send(Ctx, Data),
    receive
        {Ref, Resp} ->
            Resp;
        {message, Resp} ->
            {message, Resp}
    after Timeout ->
        throw({error, timeout, Ref})
    end.


%% Internal API

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
