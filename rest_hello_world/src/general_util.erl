%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2022 12:58 pm
%%%-------------------------------------------------------------------
-module(general_util).
-author("ganesonravichandran").

%% API
-export([create_a_string/1, create_a_random_string/1, create_random_tweet/2]).

%% Util to create a random string.
create_random_string(ByteLength) ->
  binary:bin_to_list(
    base64:encode(
      crypto:strong_rand_bytes(
        ByteLength
      )
    )
  ).

%%% Creates a tweet with the word lengths.
%%% Can pass mention and hash tag as input.
create_a_string(WordLengths) ->
  Words = lists:map(
    fun({StrLength, Type, Name}) ->
      case Type of
        mention -> string:join(["@", Name], "");
        hashtag -> string:join(["#", Name], "");
        _ -> create_random_string(StrLength)
      end
    end,
    WordLengths
  ),
  string:join(Words, " ").

create_a_random_string(CharLength) ->
  create_random_string(CharLength).

create_random_tweet(0, Acc) -> Acc;
create_random_tweet(N, Acc) -> create_random_tweet(N - 1, lists:append([
  create_random_string(
    rand:uniform(80)
  )
], Acc)).