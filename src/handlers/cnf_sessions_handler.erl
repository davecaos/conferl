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

-module(cnf_sessions_handler).

-author('David Cao <david.c.h.cao@gmail.com>').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {cnf_default_handler,
         [ init/2
         , rest_terminate/2
         , content_types_accepted/2
         , content_types_provided/2
         , is_authorized_by_password/2
         ]}
       ]).

-export([handle_post/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([trails/0]).

-type state() :: #{}.

%% trails_handler callback
-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ post =>
       #{ tags => ["sessions"]
        , description => "Post a new session"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        }
     },
  Path = "/sessions/",
  Options = #{module => ?MODULE, init_args => #{path => Path}},
  [trails:trail(Path, ?MODULE, Options, Metadata)].


allowed_methods(Req, State) ->
  {[ <<"POST">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_password(Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
  {true, cowboy_req:req(), state()}
  | {tuple(), cowboy_req:req(), state()}.
handle_post(Req, State) ->
  #{user_name := UserName} = State,
  User = cnf_user_repo:find_by_name(UserName),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  JsonBody = cnf_session:to_json(Session),
  Req1 = cowboy_req:set_resp_body(JsonBody, Req),
  {true, Req1, State}.
