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
-module(cnf_message).

-author('David Cao <david.cao@inakanetworks.com>').

-opaque message() ::
  #{ id           => integer()
   , content_id   => integer()
   , response_id  => integer()
   , message_text => binary()
   , user_id      => integer()
   , created_at   => tuple()
   , updated_at   => tuple()
   }.

-export_type([message/0]).

%%% sumo_db callbacks
-export([sumo_schema/0]).
-export([sumo_wakeup/1]).
-export([sumo_sleep/1]).

-export([new/4]).
-export([id/1]).
-export([content_id/1]).
-export([response_id/1]).
-export([message_text/1]).
-export([message_text/2]).
-export([user_id/1]).
-export([user_id/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([is_top_message/1]).
-export([updated_at/1]).
-export([updated_at/2]).
-export([score/1]).
-export([score/2]).
-export([to_json/1]).

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

%%% sumo_db callbacks

-spec sumo_wakeup(sumo:doc()) -> message().
sumo_wakeup(Data) -> truncate_seconds(replace_null(Data)).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(message()) -> sumo:doc().
sumo_sleep(Message) -> Message.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id          , integer,  [id, auto_increment, not_null]),
    sumo:new_field(content_id  , integer,  [not_null]),
    sumo:new_field(response_id , integer,  []),
    sumo:new_field(message_text, string ,  [{length, 1024}, not_null]),
    sumo:new_field(user_id     , integer,  [not_null]),
    sumo:new_field(score       , integer,  [not_null]),
    sumo:new_field(created_at  , datetime, [not_null]),
    sumo:new_field(updated_at  , datetime, [not_null])
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for message

-spec new( integer(), integer() | undefined, string(), integer()) -> message().
new(ContentId, ResponseId, MessageText, User) ->
  Now = calendar:universal_time(),
  #{  id           => undefined
    , content_id   => ContentId
    , response_id  => ResponseId
    , message_text => MessageText
    , user_id      => User
    , score        => 0
    , created_at   => Now
    , updated_at   => Now
    }.

%% Getters/Setters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec id(message()) -> integer().
id(Message) ->
  maps:get(id, Message).

-spec content_id(message()) -> integer().
content_id(Message) ->
  maps:get(content_id, Message).

-spec response_id(message()) -> integer() | undefined.
response_id(Message) ->
  maps:get(response_id, Message).

-spec message_text(message()) -> string().
message_text(Message) ->
  maps:get(message_text, Message).

-spec message_text(message(), string()) -> message().
message_text(Message, MessageText) ->
 Message#{ message_text => MessageText}.

-spec user_id(message()) -> string().
user_id(Message) ->
  maps:get(user_id, Message).

-spec user_id(message(), string()) -> message().
user_id(Message, MessageText) ->
 Message#{user_id => MessageText}.

-spec created_at(message()) -> tuple().
created_at(Message) ->
  maps:get(created_at, Message).

-spec created_at(message(), tuple()) -> message().
created_at(Message, CreatedAt) ->
  Message#{created_at => CreatedAt}.

-spec updated_at(message()) -> tuple().
updated_at(Message) ->
  maps:get(updated_at, Message).

-spec updated_at(message(), tuple()) -> message().
updated_at(Message, UpdatedAt) ->
  Message#{updated_at => UpdatedAt}.

-spec score(message()) -> integer().
score(Message) ->
  maps:get(score, Message).

-spec score(message(), integer()) -> message().
score(Message, Score) ->
  Message#{score => Score}.

-spec is_top_message(message()) -> boolean().
is_top_message(Message) ->
  response_id(Message) == undefined.

-spec to_json(message() | [message()]) -> message() | [message()].
to_json(Message) when is_map(Message) ->
  jiffy:encode(doc_to_binary_date(Message));
to_json(ListMessages) when is_list(ListMessages) ->
  JsonListMessages = [doc_to_binary_date(Message) || Message <- ListMessages],
  jiffy:encode(JsonListMessages).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
-spec replace_null(sumo:doc()) -> message().
replace_null(Message = #{response_id := null}) ->
  Message#{response_id => undefined};
replace_null(Message) -> Message.

-spec doc_to_binary_date(map()) -> map().
doc_to_binary_date(Message) ->
  CreatedAtBinary = cnf_utils:datetime_to_binary(created_at(Message)),
  UpdatedAtBinary = cnf_utils:datetime_to_binary(updated_at(Message)),
  Message#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.

-spec truncate_seconds(message()) -> message().
truncate_seconds(Message) ->
  CreatedAt = cnf_utils:truncate_seconds(created_at(Message)),
  UpdatedAt = cnf_utils:truncate_seconds(updated_at(Message)),
  Message#{updated_at => UpdatedAt, created_at => CreatedAt}.
