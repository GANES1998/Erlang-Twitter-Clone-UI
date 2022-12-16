%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2022 11:04 am
%%%-------------------------------------------------------------------

-record(tweet_user, {
  user_id,
  login,
  current_pid
}).

-record(tweets, {
  tweet_id,
  poster,
  content,
  timestamp
}).

-record(tweet_hash_tag, {
  hash_tag,
  tweet_id
}).

-record(tweet_mensions, {
  mension,
  tweet_id
}).

-record(user_feed, {
  user_id,
  type,
  tweet_id,
  viewed
}).

-record(subscribers, {
  user_id,
  subscriber
}).

-record(retweet, {
  user_id,
  tweet_id,
  timestamp
}).


