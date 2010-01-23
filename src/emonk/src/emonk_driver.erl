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

-export([start/0, new/0, destroy/1, shutdown/1]).

-define(SCRIPT_TIMEOUT, 5000).
-define(DRIVER_NAME, "emonk_drv").
-define(PORT_OPTS, [binary]).

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
    {ok, open_port({spawn_driver, ?DRIVER_NAME ++ " foo"}, ?PORT_OPTS)}.

destroy(Ctx) ->
    case (catch port_close(Ctx)) of
        true -> ok;
        Else -> Else
    end.

shutdown(Ctx) ->
    call_driver(Ctx, "sd", [], 60000),
    port_close(Ctx).

call_driver(Ctx, Command, Args, Timeout) ->
    CallToken = make_call_token(),
    Marshalled = js_drv_comm:pack(Command, [CallToken] ++ Args),
    port_command(Ctx, Marshalled),
    Result = receive
                 {CallToken, ok} ->
                     ok;
                 {CallToken, ok, R} ->
                     {ok, R};
                 {CallToken, error, Error} ->
                     {error, Error}
             after Timeout ->
                     {error, timeout}
             end,
    Result.

make_call_token() ->
    list_to_binary(integer_to_list(erlang:phash2(erlang:make_ref()))).
