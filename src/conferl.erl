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
-export([start_phase/3]).

%% application
%% @doc Starts the application
start() ->
  {ok, _Started} = application:ensure_all_started(conferl).

%% @doc Stops the application
stop() ->
  application:stop(conferl).

%% behaviour
%% @private
-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_Type, _Args) -> conferl_sup:start_link().

start_phase(start_cowboy_listeners, _StartType, []) ->
  Handlers =
  [ cowboy_swagger_handler
  , cnf_status_handler
  , cnf_contents_handler
  , cnf_content_single_handler
  , cnf_sessions_handler
  , cnf_session_single_handler
  , cnf_users_handler
  , cnf_user_single_handler
  , cnf_messages_handler
  , cnf_single_message_handler
  , cnf_me_handler
    %% Add here new handlers
  ],
  
  % Get the trails routes for each handler
  Routes = trails:trails(Handlers),

  % Store them in trails so Cowboy is able to get them
  trails:store(Routes),

  % Set server routes
  Dispatch = trails:single_host_compile(Routes),

  % Set the options for cowboy
  {ok, Port} = application:get_env(conferl, http_port),
  {ok, HttpListenersCount} = application:get_env(conferl, http_listener_count),
  TransOptions = [{port, Port}],
  ProtoOptions = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  
  % Start Cowboy HTTP server
  case cowboy:start_http(http_conferl_server, HttpListenersCount, TransOptions, ProtoOptions) of
    {error, {already_started, _}} -> ok;
    _WhenOthers -> ok
  end.

%% @private
stop(_State) ->
  ok.
