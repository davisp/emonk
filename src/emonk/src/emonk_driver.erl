%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(emonk_driver).

-export([start/0, new/0, new/1, destroy/1]).
-export([call_driver/2, call_driver/3]).

-define(SCRIPT_TIMEOUT, 5000).
-define(DRIVER_NAME, "emonk_drv").
-define(PORT_OPTS, [binary]).

-define(RT_MAX_BYTES, 1048576).
-define(GC_MAX_BYTES, 8388608).
-define(GC_MAX_MALLOC, 8388608).
-define(CONTEXT_STACK, 8192).

start() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
        false ->
            case erl_ddll:load(code:priv_dir(emonk), ?DRIVER_NAME) of
                ok ->
                    true;
                {error, Error} ->
                    {error, {?DRIVER_NAME, erl_ddll:format_error(Error)}}
            end;
        true ->
            true
    end.

new() ->
    {ok, open_port({spawn_driver, ?DRIVER_NAME}, [binary])}.

new(Settings) ->
    {ok, open_port({spawn_driver, parse_settings(Settings)}, [binary])}.

destroy(Ctx) ->
    case (catch port_close(Ctx)) of
        true -> ok;
        Else -> Else
    end.

call_driver(Port, Command) ->
    call_driver(Port, Command, ?SCRIPT_TIMEOUT).

call_driver(Port, Command, Timeout) ->
    Token = make_call_token(),
    case port_control(Port, 0, term_to_binary({Token, Command})) of
        [0] ->
            {error, driver_error};
        [] ->
            receive
                {Token, ok, undefined} -> {ok, undefined};
                {Token, ok, Resp} -> {ok, Resp};
                {Token, error, Error} -> {error, Error};
                Else -> io:format(standard_error, "UNEXPECTED: ~p", [Else])
            after Timeout ->
                {error, timeout}
            end
    end.

parse_settings(Settings) when is_list(Settings) ->
    RtMaxBytes = proplists:get_value(rt_max_bytes, Settings, ?RT_MAX_BYTES),
    GcMaxBytes = proplists:get_value(gc_max_bytes, Settings, ?GC_MAX_BYTES),
    GcMaxMalloc = proplists:get_value(gc_max_malloc, Settings, ?GC_MAX_MALLOC),
    CtxStack = proplists:get_value(context_stack, Settings, ?CONTEXT_STACK),
    Cmd = io_lib:format(
        "~s rt=~B gcmb=~B gcld=~B ctx=~B",
        [?DRIVER_NAME, RtMaxBytes, GcMaxBytes, GcMaxMalloc, CtxStack]
    ),
    lists:flatten(Cmd).

make_call_token() ->
    list_to_binary(integer_to_list(erlang:phash2(erlang:make_ref()))).


