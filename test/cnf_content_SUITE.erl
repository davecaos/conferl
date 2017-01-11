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

-module(cnf_content_SUITE).

-author('David Cao <david.c.h.cao@gmail.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , test_create_content/1
        , test_create_user_bad/1
        , double_registration_bad/1
        , test_list_contents/1
        , fetch_notfound_content/1
        ]).

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
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sumo_db),
  sumo:create_schema(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_content),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(test_create_content, Config) ->
  [{url, "http://inaka.net/"}, {user, 10} | Config];

init_per_testcase(test_create_user_bad, Config) ->
  [ {url, "bad_url!!!!!"}, {user, 10} | Config];

init_per_testcase(double_registration_bad, Config) ->
   [{url, "http://inaka.net/"}, {user, 10} | Config];
init_per_testcase(fetch_notfound_content, Config)  ->
  [{id, 999999} | Config];
init_per_testcase(test_list_contents, Config)  ->
  [{urls, [{"http://inaka.net/11", 11}
        ,  {"http://inaka.net/12", 12}
        ,  {"http://inaka.net/13", 13}
        ,  {"http://yahoo.com/",   14}
        ,  {"https://github.com",  15}
        ]}
  , {domain, "inaka.net"} | Config ];

init_per_testcase(_, Config)  ->
  Config.

%% @doc tests for new content
-spec test_create_content(config()) -> ok.
test_create_content(Config) ->
  Url     = proplists:get_value(url, Config),
  User    = proplists:get_value(user, Config),
  _Content = cnf_content:new(Url, User),
  ok.

-spec test_create_user_bad(config()) -> ok.
test_create_user_bad(Config) ->
  Url     = proplists:get_value(url, Config),
  User    = proplists:get_value(user, Config),
  try cnf_content:new(Url, User) of
    _NotExpected -> ct:fail("Unexpected result (!)")
  catch
    throw:invalid_url -> ok
  end,
  ok.

%% @doc tests for register
-spec double_registration_bad(config()) -> ok.
double_registration_bad(Config) ->
  Url      = proplists:get_value(url, Config),
  User     = proplists:get_value(user, Config),
  _Content = cnf_content_repo:register(Url, User),
  duplicated_content = cnf_content_repo:register(Url, User).

%% @doc tests for fetch of content
-spec fetch_notfound_content(config()) -> ok.
fetch_notfound_content(Config) ->
  ContentId   = proplists:get_value(id, Config),
  notfound = cnf_content_repo:fetch(ContentId).

%% @doc tests for list of contents
-spec test_list_contents(config()) -> ok.
test_list_contents(Config) ->
  Urls      = proplists:get_value(urls, Config),
  _Contents = [cnf_content_repo:register(Url, User) || {Url, User} <- Urls ],
  Domain    = proplists:get_value(domain, Config),
  Contents  = cnf_content_repo:list(Domain),
  FilterFun =
    fun(Cont) ->
      maps:get(domain, Cont) == proplists:get_value(domain, Config)
    end,
  true = lists:all(FilterFun, Contents),
  ok.
