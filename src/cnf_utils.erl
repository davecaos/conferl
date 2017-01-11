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
-module(cnf_utils).

-author('David Cao <david.c.h.cao@gmail.com>').

-export([handle_exception/3]).
-export([datetime_to_binary/1]).
-export([truncate_seconds/1]).
-export([resource_exists/3]).

-spec truncate_seconds(tuple()) -> tuple().
truncate_seconds({{Yi, Mi, Di}, {Hi, Ni, Si}}) ->
  {{Yi, Mi, Di}, {Hi, Ni, trunc(Si)}}.

-spec datetime_to_binary(tuple()) -> binary().
datetime_to_binary({{Yi, Mi, Di}, {Hi, Ni, Si}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  %% epgsql uses {Hour, Minute, Second.Microsecond}
  S = integer_to_list(Si),
  iolist_to_binary([Y, "-", M, "-", D, "T", H, ":", N, ":", S, ".000000Z"]).

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
  {halt, cowboy_req:req(), term()}
  | {{false, binary()}, cowboy_req:req(), term()}.
handle_exception(duplicated_user, Req, State) ->
  %{false, Req1, State} is equivalent-> {ok, Req1} = cowboy_req:reply(400, Req),
  {false, Req, State};
handle_exception(wrong_password, Req, State) ->
  {{false, <<"Basic realm=\"conferl\"">>}, Req, State};
handle_exception(not_registered, Req, State) ->
  {{false, <<"Basic realm=\"conferl\"">>}, Req, State};
handle_exception(duplicated_content, Req, State) ->
  {false, Req, State};
handle_exception(not_found, Req, State) ->
  Req1 = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(Reason, Req, State) ->
  Req1 =
    try cowboy_req:reply(501, Req)
    catch
      _:Error ->
        _ = lager:critical(
          "~p trying to report error through cowboy. Stack T: ~p Reason: ~p",
          [Error, erlang:get_stacktrace(), Reason]),
        Req
    end,
  {halt, Req1, State}.

-spec resource_exists(fun((any()) -> any()), cowboy_req:req(), map()) ->
  {boolean(), cowboy_req:req(), map()}.
resource_exists(Fetch, Req, State) ->
  case cowboy_req:binding(id, Req) of
    undefined ->
      {true, Req, State};
    Id ->
      Method = cowboy_req:method(Req),
      resource_exists(Method, Id, Fetch, Req, State)
  end.


resource_exists(<<"DELETE">>, _, _Fetch, Req, State) -> {true, Req, State};
resource_exists(_, Id, Fetch, Req, State) ->
  try Fetch(binary_to_integer(Id)) of
    notfound -> {false, Req, State};
    Resource -> {true, Req, State#{resource => Resource}}
  catch
    _Error:_Exp -> {false, Req, State}
  end.
