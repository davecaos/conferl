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

-module(cnf_messages_handler).

-author('David Cao <david.c.h.cao@gmail.com>').

-include_lib("mixer/include/mixer.hrl").

-behaviour(trails_handler).

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
-export([handle_post/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([trails/0]).

-type state() :: #{}.

%% trails_handler callback
-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["messages"]
        , description => "Get messages"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["messages"]
        , description => "Post a message"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        }
     },
  Path = "/messages/",
  Options = #{module => ?MODULE, init_args => #{path => Path}},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>]
  , Req
  , State
  }.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_token(Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | true, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  {ok, JsonBody, Req1} = cowboy_req:body(Req),
  Body = jiffy:decode(JsonBody, [return_maps]),
  #{ <<"in_reply_to">> := ReplayMsgId
   , <<"message">> :=  Message
   , <<"content">> :=  ContentId} = Body,
  try
    #{token := Token} = State,
    UserId  =
      cnf_session:user_id(cnf_session_repo:find_by_token(Token)),
    MsgResponse =
      cnf_message_repo:write_reply(ContentId, ReplayMsgId, Message, UserId),
    JsonRespBody = cnf_message:to_json(MsgResponse),
    Req2 = cowboy_req:set_resp_body(JsonRespBody, Req1),
    {true, Req2, State}
  catch
    _Type:Exception ->
      cnf_utils:handle_exception(Exception, Req, State)
  end.

all_msg_content(Value) ->
  BuilderFun =
    fun(ContentId) ->[{content_id, ContentId}] end,
  binary_number(Value, BuilderFun).

top_msg_content(Value) ->
  BuilderFun =
    fun(ContentId) -> [{content_id, ContentId}, {response_id, null}] end,
  binary_number(Value, BuilderFun).

all_rply_content(Value) ->
  BuilderFun =
    fun(ContentId) -> [{content_id, ContentId}, {response_id, not_null}] end,
  binary_number(Value, BuilderFun).

all_msg_user(Value) ->
  BuilderFun = fun(UserId) -> [{user_id, UserId}] end,
  binary_number(Value, BuilderFun).

sort_created_at(<<"true">>) ->
  {true, [{created_at, desc}]};
sort_created_at(_) ->
  false.

sort_by_score(<<"true">>) ->
  {true, [{score, desc}]};
sort_by_score(_)->
  false.

binary_number(Binary, BuilderFun) ->
  try binary_to_integer(Binary) of
    Value -> {true, BuilderFun(Value)}
  catch
    _Error:_Exp -> []
  end.

-spec handle_get(cowboy_req:req(), state()) ->
  {list(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{ all_msg_content := AllMsgContest
   , all_rply_content := AllRplyContent
   , top_msg_content := TopMsgContent
   , all_msg_user := AllMsgUser
   , sort_created_at := SortCreatedAt
   , sort_by_score := SortByScore
   } = cowboy_req:match_qs([ {all_msg_content, fun all_msg_content/1 , []}
                           , {all_rply_content, fun all_rply_content/1, []}
                           , {top_msg_content, fun top_msg_content/1, []}
                           , {all_msg_user, fun all_msg_user/1, []}
                           , {sort_created_at, fun sort_created_at/1, []}
                           , {sort_by_score, fun sort_by_score/1, []}
                           ], Req),
  QueryString =
    AllMsgContest ++
    AllMsgUser ++
    AllRplyContent ++
    TopMsgContent ++
    AllMsgUser,
  OrderString = SortCreatedAt ++ SortByScore,
  RequestContent = cnf_message_repo:custom_query(QueryString, OrderString),
  Fun1 =
    fun (Message) ->
      ListVotes = cnf_vote_repo:list_votes(cnf_message:id(Message)),
      Message#{votes => ListVotes}
    end,
  RequestContent2 = lists:map(Fun1, RequestContent),
  Body = cnf_content:to_json(RequestContent2),
  {Body, Req, State}.
