-module(couch_jwt_auth).
-export([decode/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("couch/include/couch_db.hrl").

-spec decode(Token :: binary(), Config :: list()) -> list().
decode(Token, Config) ->
  BlacklistStr = couch_util:get_value("blacklist", Config, "[]"),
  {ok, Blacklist} = erl_parse:parse_term(BlacklistStr),
  Blacklisted = lists:member(Token, Blacklist),
  if
    Blacklisted -> throw(token_rejected);
    true -> io:fwrite('good')
  end.

-ifdef(TEST).

-define (BlacklistConfig, [{"hs_secret","c2VjcmV0"}, {"blacklist","[\"blacklisted-token\"]"}]).

decode_blacklist_test() ->
  ?assertThrow(token_rejected, decode("blacklisted-token", ?BlacklistConfig)).

-endif.
