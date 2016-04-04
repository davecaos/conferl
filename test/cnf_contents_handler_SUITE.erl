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

-module(cnf_contents_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_handle_post_ok/1]).
-export([test_get_qs_ok/1]).
-export([test_handle_post_duplicated/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
-spec ignored_funs() -> [atom()].
ignored_funs() ->
  [ module_info
  , init_per_suite
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(conferl),
  {ok, _} = application:ensure_all_started(shotgun),
  sumo:create_schema(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_content),
  sumo:delete_all(cnf_user),
  Config.

%% @doc definion of init_per_testcases

init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases

end_per_testcase(_Function, Config) ->
  Config.

-spec test_handle_post_ok(config()) -> config().
test_handle_post_ok(Config) ->
  User =
    cnf_user_repo:register(<<"post_ok">>, <<"pass">>, <<"mail@email.net">>),
  Id = cnf_user:id(User),
  Session = cnf_session_repo:register(Id),
  Token = binary_to_list(cnf_session:token(Session)),
  Header =
   #{ <<"Content-Type">> => <<"application/json">>
    , basic_auth => {"post_ok", Token}
    },
  Body = #{ url => <<"http://inaka.net/post_ok">>},
  JsonBody = jiffy:encode(Body),
  {ok, Response} =
  cnf_test_utils:api_call(post, "/contents", Header,  JsonBody),
  #{status_code := 204} = Response,
  #{headers := ResponseHeaders} = Response,
  Location = proplists:get_value(<<"location">>, ResponseHeaders),
  Url = cnf_test_utils:get_server_url(),
  UrlSize =  byte_size(Url),
  EndpointUrl = <<Url:UrlSize/binary, "/contents/">>,
  Size = byte_size(EndpointUrl),
  <<EndpointUrl:Size/binary, _Id/binary>> = Location,
  Config.

-spec test_handle_post_duplicated(config()) -> config().
test_handle_post_duplicated(Config) ->
  User =
    cnf_user_repo:register(<<"post_dupl">>, <<"pass">>, <<"mail@email.net">>),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  Header =
   #{ <<"Content-Type">> => <<"application/json">>
    , basic_auth => {"post_dupl", Token}
    },
  Body = #{ url => <<"http://inaka.net/post_dup">>},
  JsonBody = jiffy:encode(Body),
  Id = cnf_user:id(User),
  _ = cnf_content_repo:register("http://inaka.net/post_dup", Id),
  {ok, Response} =
    cnf_test_utils:api_call(post, "/contents", Header,  JsonBody),
  #{status_code := 400} = Response,
  Config.

-spec test_get_qs_ok(config()) -> config().
test_get_qs_ok(Config) ->
  User    = cnf_user_repo:register("get_qs_ok", "pass", "mail@email.net"),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token   = binary_to_list(cnf_session:token(Session)),
  Header =
   #{ <<"Content-Type">> => <<"text/plain; charset=utf-8">>
    , basic_auth => {"get_qs_ok", Token}
    },
  UrlPostInaka   = "http://inaka.net/get_qs_ok",
  UrlPostTwitter = "https://twitter.com/get_qs_ok",
  ContentInaka   = cnf_content_repo:register(UrlPostInaka, 1),
  ContentTwitter = cnf_content_repo:register(UrlPostTwitter, 1),
  DomainInaka    = "inaka.net" = cnf_content:domain(ContentInaka),
  DomainTwitter  = "twitter.com" = cnf_content:domain(ContentTwitter),
  UrlInaka = "/contents/?domain=" ++ DomainInaka,
  {ok, ResponseInaka} = cnf_test_utils:api_call(get, UrlInaka, Header),
  #{status_code := 200} = ResponseInaka,
  #{body := JsonBodyRespInaka} = ResponseInaka,
  BodyRespInaka = jiffy:decode(JsonBodyRespInaka, [return_maps]),
  UrlTwitter = "/contents/?domain=" ++ DomainTwitter,
  F1 =
   fun(DomainMap) ->
      #{<<"domain">> := Domain} = DomainMap,
      Domain = <<"inaka.net">>
   end,
  ok = lists:foreach(F1, BodyRespInaka),
  {ok, ResponseTwitter} = cnf_test_utils:api_call(get, UrlTwitter, Header),
  #{body := JsonBodyRespTwitter} = ResponseTwitter,
  BodyRespTwitter = jiffy:decode(JsonBodyRespTwitter, [return_maps]),
  F2 =
   fun(DomainMap) ->
     #{<<"domain">> := Domain} = DomainMap,
     Domain = <<"twitter.com">>
   end,
  ok = lists:foreach(F2, BodyRespTwitter),
  #{status_code := 200} = ResponseTwitter,
  Config.
