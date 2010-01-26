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

%% @ doc This module is the entry point to start erlang_js as an OTP application.
-module(emonk).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-export([new/0, new/1, destroy/1]).
-export([eval/2, eval/3, call/3, call/4]).

-define(SCRIPT_TIMEOUT, 5000).
-define(PORT_OPTS, [binary]).
-define(DRIVER_NAME, "emonk_drv").

-define(RT_MAX_BYTES, 1048576).
-define(GC_MAX_BYTES, 8388608).
-define(GC_MAX_MALLOC, 8388608).
-define(CONTEXT_STACK, 8192).

%% @spec start() -> ok | {error, any()}
%% @doc Starts the emonk OTP applicatio. Intended for
%% use with the Erlang VM's -s option
start() ->
    application:start(emonk).

%% @spec new() -> {ok, Port}
%% @doc Create a new Port which serves as a context
%% for executing JavaScript code.
new() ->
    {ok, open_port({spawn_driver, ?DRIVER_NAME}, [binary])}.

new(Settings) ->
    {ok, open_port({spawn_driver, parse_settings(Settings)}, [binary])}.

destroy(Ctx) ->
    case (catch port_close(Ctx)) of
        true -> ok;
        Else -> Else
    end.

eval(Port, Script) ->
    call_driver(Port, 0, Script, ?SCRIPT_TIMEOUT).

eval(Port, Script, Timeout) ->
    call_driver(Port, 0, Script, Timeout).

call(Port, FunctionName, Args) ->
    call_driver(Port, 1, {FunctionName, Args}, ?SCRIPT_TIMEOUT).

call(Port, FunctionName, Args, Timeout) ->
    call_driver(Port, 1, {FunctionName, Args}, Timeout).

%% @private
start(_StartType, _StartArgs) ->
    emonk_sup:start_link().

%% @private
stop(_State) ->
    ok.

%% @private
call_driver(Port, Command, Data, Timeout) ->
    Token = make_call_token(),
    %io:format(standard_error, "~p~n", [term_to_binary({Token, Data})]),
    case port_control(Port, Command, term_to_binary({Token, Data})) of
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

%% @private
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

%% @private
make_call_token() ->
    list_to_binary(integer_to_list(erlang:phash2(erlang:make_ref()))).
