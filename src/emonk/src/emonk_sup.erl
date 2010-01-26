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

%% @doc Top level supervisor for erlang_js. It is chiefly responsible for
%% the js_cache file cache process.
-module(emonk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DRIVER_NAME, "emonk_drv").

%% @private
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @private
init([]) ->
    Flags = {one_for_one, 1000, 3600},
    CacheMFA = {emonk_cache, start_link, []},
    Kids = [{cache, CacheMFA, permanent, 2000, worker, [emonk_cache]}],
    case load_driver() of
        true -> {ok, {Flags, Kids}};
        false -> throw({error, {load_error, "Failed to load emonk_drv.so"}})
    end.

load_driver() ->
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