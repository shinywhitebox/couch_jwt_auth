%   Copyright 2015 Matti Eerola
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

-module(couch_jwt_auth).
-export([jwt_authentication_handler/1]).
-export([decode/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("couch/include/couch_db.hrl").

-import(couch_httpd, [header_value/2]).

%% @doc token authentication handler.
%
% This handler allows creation of a userCtx object from a JSON Web Token (JWT).
jwt_authentication_handler(Req) ->
  case header_value(Req, "Authorization") of
    "Bearer " ++ Token -> 
      try
        ensure_safe_token(Token, couch_config:get("jwt_auth_blacklist")),
        token_auth_user(Req, decode(Token))
      catch
        % return generic error message (https://www.owasp.org/index.php/Authentication_Cheat_Sheet#Authentication_Responses)
        throw:_ -> throw({unauthorized, <<"Token rejected">>});
        error:_ -> throw({unauthorized, <<"Token rejected">>})
      end;
    _ -> Req
  end.

%% @doc decode and validate JWT using CouchDB config
-spec decode(Token :: binary()) -> list().
decode(Token) ->
  decode(Token, couch_config:get("jwt_auth")).

% Config is list of key value pairs:
% [{"secret","..."},{"roles_claim","roles"},{"name_claim","name"}]
-spec decode(Token :: binary(), Config :: list()) -> list().
decode(Token, Config) ->
  Secret = couch_util:get_value("secret", Config),
  case List = ejwt:decode(list_to_binary(Token), Secret) of
    error -> throw(signature_not_valid);
    _ -> validate(lists:map(fun({Key, Value}) ->
        {?b2l(Key), Value}
      end, List), erlang:system_time(seconds), Config)
  end.

ensure_safe_token(Token, Config) ->
  case couch_util:get_value(Token, Config) of
    undefined -> true;
    Reason -> throw(Reason)
  end.

readValidationConfig(Config) -> 
  ClaimsConfig = couch_util:get_value("validated_claims", Config, ""),
  Claims = string:tokens(ClaimsConfig,","),
    lists:map(fun(ClaimName) ->
        {ClaimName, couch_util:get_value(string:concat("validate_claim_", ClaimName), Config)}
      end, Claims). 

validate(TokenInfo, NowSeconds, Config) ->
  ValidationConfig = readValidationConfig(Config),
  Expiration = couch_util:get_value("exp", TokenInfo),
  NotBefore = couch_util:get_value("nbf", TokenInfo),
  NotValid = lists:any(fun({ClaimName, ValidValuesJSON}) ->
        ValidValues = ?JSON_DECODE(ValidValuesJSON),
        Values = case couch_util:get_value(ClaimName, TokenInfo) of
          List when is_list(List) -> List;
          _String -> [_String]
        end,
        lists:all(fun(Value) ->
          not lists:member(Value, ValidValues)
        end, Values)
      end, ValidationConfig),
  if
    NotValid -> throw(token_rejected);
    (Expiration /= undefined) and (Expiration =< NowSeconds) -> throw(token_rejected);
    (NotBefore /= undefined) and (NowSeconds < NotBefore) -> throw(token_rejected);
    true -> TokenInfo
  end.
    
token_auth_user(Req, User) ->
  {Name, Roles} = get_userinfo_from_token(User, couch_config:get("jwt_auth")),
  Req#httpd{user_ctx=#user_ctx{name=Name, roles=Roles}}.

get_userinfo_from_token(User, Config) ->
  Name = couch_util:get_value(couch_util:get_value("name_claim", Config, "name"), User, null),
  Roles = couch_util:get_value(couch_util:get_value("roles_claim", Config, "roles"), User, []),
  {Name, Roles}.

% UNIT TESTS
-ifdef(TEST).

-define (EmptyConfig, [{"secret",""}]).
-define (BasicConfig, [{"secret","secret"}]).
-define (BlacklistConfig, [{"token","bad guy 1"}]).
-define (BasicTokenInfo, [{"name",<<"John Doe">>},{"admin",true}]).
-define (BasicToken, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1lIjoiSm9obiBEb2UiLCJhZG1pbiI6dHJ1ZX0.OLvs36KmqB9cmsUrMpUutfhV52_iSz4bQMYJjkI_TLQ").

decode_malformed_empty_test() ->
  ?assertError({badmatch,_}, decode("", ?EmptyConfig)).

decode_malformed_dots_test() ->
  ?assertError({badarg,_}, decode("...", ?EmptyConfig)).

decode_malformed_nosignature1_test() ->
  ?assertError({badmatch,_}, decode("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOmZhbHNlfQ", ?BasicConfig)).

decode_malformed_nosignature2_test() ->
  ?assertThrow(signature_not_valid, decode("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.", ?BasicConfig)).

decode_simple_test() ->
  TokenInfo = ?BasicTokenInfo,
  ?assertEqual(TokenInfo, decode(?BasicToken, ?BasicConfig)).

decode_unsecured_test() ->
  ?assertError(function_clause, decode("eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJuYW1lIjoiSm9obiBEb2UiLCJhZG1pbiI6dHJ1ZX0.", ?BasicConfig)).

ensure_safe_token_ok_test() ->
  ?assertEqual(true, ensure_safe_token("good token", ?BlacklistConfig)).

ensure_safe_token_unsafe_test() ->
  ?assertThrow("bad guy 1", ensure_safe_token("token", ?BlacklistConfig)).

validate_simple_test() ->
  TokenInfo = ?BasicTokenInfo,
  ?assertEqual(TokenInfo, validate(TokenInfo, 1000, ?EmptyConfig)).

validate_exp_nbf_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"exp",2000}, {"nbf",900}]]),
  ?assertEqual(TokenInfo, validate(TokenInfo, 1000, ?EmptyConfig)).

validate_exp_rejected_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"exp",2000}]]),
  ?assertThrow(token_rejected, validate(TokenInfo, 3000, ?EmptyConfig)).

validate_nbf_rejected_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"nbf",2000}, {"exp",3000}]]),
  ?assertThrow(token_rejected, validate(TokenInfo, 1000, ?EmptyConfig)).

validate_aud1_rejected_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"aud",<<"123">>}]]),
  Config = lists:append([?EmptyConfig, [{"validated_claims", "aud"}, {"validate_claim_aud", "[\"456\"]"}]]),
  ?assertThrow(token_rejected, validate(TokenInfo, 1000, Config)).

validate_aud2_rejected_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"aud",[<<"123">>,<<"234">>]}]]),
  Config = lists:append([?EmptyConfig, [{"validated_claims", "aud"}, {"validate_claim_aud", "[\"456\"]"}]]),
  ?assertThrow(token_rejected, validate(TokenInfo, 1000, Config)).

validate_aud_pass_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"aud",[<<"123">>,<<"234">>]}]]),
  Config = lists:append([?EmptyConfig, [{"validated_claims", "aud"}, {"validate_claim_aud", "[\"123\",\"456\"]"}]]),
  ?assertEqual(TokenInfo, validate(TokenInfo, 1000, Config)).

validate_claims_pass_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"aud",<<"123">>}, {"iss",<<"abc">>}]]),
  Config = lists:append([?EmptyConfig, [{"validated_claims", "aud,iss"}, {"validate_claim_aud", "[\"123\"]"},{"validate_claim_iss", "[\"abc\"]"}]]),
  ?assertEqual(TokenInfo, validate(TokenInfo, 1000, Config)).

get_userinfo_from_token_default_test() ->
  TokenInfo = ?BasicTokenInfo,
  {Name, Roles} = get_userinfo_from_token(TokenInfo, ?EmptyConfig),
  ?assertEqual([], Roles),
  ?assertEqual(<<"John Doe">>, Name).

get_userinfo_from_token_configured_test() ->
  TokenInfo = ?BasicTokenInfo,
  Config = lists:append([?EmptyConfig, [{"name_claim", "name"}]]),
  {Name, Roles} = get_userinfo_from_token(TokenInfo, Config),
  ?assertEqual([], Roles),
  ?assertEqual(<<"John Doe">>, Name).

% user context is created with null username if username claim is not found from token
get_userinfo_from_token_name_not_found_test() ->
  TokenInfo = lists:append([?BasicTokenInfo,[{"roles",[<<"123">>]}]]),
  Config = lists:append([?EmptyConfig, [{"name_claim", "doesntexist"}]]),
  {Name, Roles} = get_userinfo_from_token(TokenInfo, Config),
  ?assertEqual([<<"123">>], Roles),
  ?assertEqual(null, Name).
-endif.
