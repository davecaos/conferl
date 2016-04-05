% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

-module(conferl).
-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).

%% application
%% @doc Starts the application
start() ->
  {ok, _Started} = application:ensure_all_started(conferl).

%% @doc Stops the application
stop() ->
  application:stop(conferl).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  EndPoints =
  [
      {<<"/status">>, cnf_status_handler, []}
    , {<<"/contents/">>, cnf_contents_handler, []}
    , {<<"/contents/:content_id">>, cnf_content_single_handler, []}
    , {<<"/sessions/">>, cnf_sessions_handler, []}
    , {<<"/sessions/:token">>, cnf_session_single_handler, []}
    , {<<"/users/">>, cnf_users_handler, []}
    , {<<"/users/:user_id">>, cnf_user_single_handler, []}
    , {<<"/me/">>, cnf_me_handler, []}
    %% Add here new endpoints
  ],
  Dispatch = cowboy_router:compile( [{'_' , EndPoints}]),
  {ok, Port} = application:get_env(conferl, http_port),
  {ok, HttpListenersCount} = application:get_env(conferl, http_listener_count),
  {ok, _} =
    cowboy:start_http( my_http_listener
                     , HttpListenersCount
                     , [{port, Port}]
                     , [{env, [{dispatch, Dispatch}]}]
                     ),

  conferl_sup:start_link().

%% @private
stop(_State) ->
  ok.
