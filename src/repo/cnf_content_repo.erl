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
-module(cnf_content_repo).
-author('David Cao <david.c.h.cao@gmail.com>').

-type maybe_exists(Type) :: Type | not_found.

%%% General repo functions.
-export(
  [ update/1
  , find/1
  , find_by_url/1
  , find_by_user/1
  , find_by_domain/1
  , register/2
  , unregister/1
  , fetch/1
  , list/1
  ]).

-spec update(cnf_content:content()) -> cnf_content:content().
update(Content) ->
  UpdatedContent = cnf_content:updated_at(Content, calendar:universal_time()),
  sumo:persist(cnf_content, UpdatedContent).

-spec find_by_url(string()) -> [cnf_content:content()].
find_by_url(Url) ->
  sumo:find_by(cnf_content, [{url, Url}]).

-spec find(non_neg_integer()) -> [cnf_content:content()].
find(ContentId)  ->
  sumo:find(cnf_content, ContentId).

-spec find_by_user(integer()) -> [cnf_content:content()].
find_by_user(UserId)  ->
  sumo:find_by(cnf_content, [{user_id, UserId}]).

-spec find_by_domain(string()) -> [cnf_content:content()].
find_by_domain(Domain)  ->
  sumo:find_by(cnf_content, [{domain, Domain}]).

-spec register(string(), integer()) ->
  cnf_content:content() | duplicated_content.
register(Url, User) ->
  Content = cnf_content:new(Url, User),
  case find_by_url(cnf_content:url(Content)) of
    []  -> sumo:persist(cnf_content, Content);
    _   -> duplicated_content
  end.

-spec unregister(non_neg_integer()) -> boolean().
unregister(Id) ->
  sumo:delete(cnf_content, Id).

-spec fetch(integer()) -> maybe_exists(cnf_content:content()).
fetch(ContentId) ->
  sumo:find(cnf_content, ContentId).

-spec list(binary()) -> [cnf_content:content()].
list(Domain) ->
  sumo:find_by(cnf_content, [{domain, Domain}]).
