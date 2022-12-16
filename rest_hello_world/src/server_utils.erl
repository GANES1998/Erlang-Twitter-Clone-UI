%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2022 6:32 pm
%%%-------------------------------------------------------------------
-module(server_utils).
-author("ganesonravichandran").

%% API
-export([get_tweet_count_from_tweets/1]).

get_tweet_count_from_tweets(Tweets) ->
  TL = length(Tweets),

  if
    TL =< 1 -> rand:uniform(15);
    true -> TL
  end.
