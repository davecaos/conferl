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

-module(cnf_user_SUITE).

-author('David Cao <david.c.h.cao@gmail.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , create_user/1
        , duplicated_user/1
        , fetch_user/1
        , fetch_user_bad/1
        , unregistrate_user_bad/1
        , unregistrate_user/1
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
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(_Config) ->
  {ok, _} = application:ensure_all_started(sumo_db),
  sumo:create_schema(),
  [].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_user),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) ->
  Config.

-spec create_user(config()) -> ok.
create_user(_Config) ->
  UserName = <<"Doge create_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register(UserName, Passsword, Email),
  UserName = cnf_user:user_name(RegistedUser),
  Passsword = cnf_user:password(RegistedUser),
  Email = cnf_user:email(RegistedUser),
  ok.

-spec fetch_user(config()) -> ok.
fetch_user(_Config) ->
  UserName = <<"Doge fetch_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register(UserName, Passsword, Email),
  Id = cnf_user:id(RegistedUser),
  PersistedUser = cnf_user_repo:find(Id),
  UserName = cnf_user:user_name(PersistedUser),
  Passsword = cnf_user:password(PersistedUser),
  Email = cnf_user:email(PersistedUser),
  ok.

-spec duplicated_user(config()) -> ok.
duplicated_user(_Config) ->
  UserName = <<"Doge duplicate_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  _User = cnf_user_repo:register(UserName, Passsword, Email),
  duplicated_user = cnf_user_repo:register(UserName, Passsword, Email),
  ok.

-spec unregistrate_user(config()) -> ok.
unregistrate_user(_Config) ->
  UserName = <<"Doge unregistrate_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register(UserName, Passsword, Email),
  cnf_user_repo:unregister(UserName),
  notfound = cnf_user_repo:find_by_name(UserName),
  ok.

-spec fetch_user_bad(config()) -> ok.
fetch_user_bad(_Config) ->
  NotFoundId = <<"0xFFFF">>,
  notfound = cnf_user_repo:find_by_name(NotFoundId),
  ok.

-spec unregistrate_user_bad(config()) -> ok.
unregistrate_user_bad(_Config) ->
  Name = <<"0xFFFF">>,
  notfound = cnf_user_repo:unregister(Name),
  ok.
