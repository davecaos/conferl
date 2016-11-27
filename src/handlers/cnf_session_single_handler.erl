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

-module(cnf_session_single_handler).

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

-export([delete_resource/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"DELETE">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_password(Req, State).

-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  Token = cowboy_req:binding(token, Req),
  #{user_name := VerifyiedUserName} = State,
  User = cnf_user_repo:find_by_name(VerifyiedUserName),
  Session = cnf_session_repo:find_by_token(Token),
  true = cnf_session:user_id(Session) == cnf_user:id(User),
  cnf_session_repo:unregister(Token),
  {true, Req, State}.
