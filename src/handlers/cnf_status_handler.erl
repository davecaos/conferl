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

-module(cnf_status_handler).

-author('David Cao <david.c.h.cao@gmail.com>').

-behaviour(trails_handler).

-export([ handle/2
        , init/2
        , trails/0
        ]).

%% trails_handler callback
-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["status"]
        , description => "Get the system status"
        , produces => ["application/json"]
        }
     },
  Path = "/status",
  Opts = #{path => Path},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

init(Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Req1 =
    cowboy_req:reply(200
                    , [{<<"content-type">>, <<"text/plain">>}]
                    , <<"{\"status\" : \"ok\"}">>
                    , Req),
  {ok, Req1, State}.

