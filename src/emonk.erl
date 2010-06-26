%% @author Kevin Smith <ksmith@basho.com>
%% @author Paul J. Davis <paul.joseph.davis@gmail.com>
%% @copyright 2009-2010 Basho Technologies
%% @copyright 2010 Paul J. Davis
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

-module(emonk).
-on_load(init/0).

-export([new_context/0, new_context/1]).
-export([eval/2, call/3]).

-define(APPNAME, emonk).
-define(LIBNAME, emonk).

new_context() ->
    not_loaded(?LINE).

new_context(_) ->
    not_loaded(?LINE).

eval(_Ctx, _Script) ->
    not_loaded(?LINE).
    
call(_Ctx, _Name, _Args) ->
    not_loaded(?LINE).

% Private API

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

