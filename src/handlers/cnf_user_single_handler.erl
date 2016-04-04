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

-module(cnf_user_single_handler).

-author('David Cao <david.cao@inakanetworks.com>').

-include_lib("inaka_mixer/include/mixer.hrl").

-mixin([
        {cnf_default_handler,
         [ init/2
         , rest_terminate/2
         , content_types_accepted/2
         , content_types_provided/2
         , is_authorized_by_token/2
         ]}
       ]).

-export([handle_get/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"GET">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) -> is_authorized_by_token(Req, State).

-spec handle_get(cowboy_req:req(), state()) ->
  {list(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  UserId = cowboy_req:binding(user_id, Req),
  RequestContent = cnf_user_repo:find(list_to_integer(binary_to_list(UserId))),
  JsonResponseBody = cnf_user:to_json(RequestContent),
  {JsonResponseBody, Req, State}.
