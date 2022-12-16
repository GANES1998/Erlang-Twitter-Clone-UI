%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2022 11:58 am
%%%-------------------------------------------------------------------
-module(db_util).
-author("ganesonravichandran").
-include("db_record.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([signup_user/1, read_user/1, signout_user/1, post_a_tweet/4, retweet/2, subscribe/2, check_user_live/1, get_tweets/1, get_feeds_for_user/1, add_feed_to_user_status/4, add_feed_to_user/3, get_tweet_ids_hashtag/1, get_tweet_ids_by_user/1, get_subscribers/1, get_tweets_from_tweet_feeds/1]).

%% When a new user joins
%% Returns the UserID
signup_user(UserPid) ->
  UserId = id_util:create_user_id(),
  F = fun() ->
    User = #tweet_user{user_id = UserId, login = true, current_pid = UserPid},
    mnesia:write(User),
    User
  end,
  _ = mnesia:transaction(F),
%%  io:format("User = ~p~n", [Us]),
  UserId.

%% Get details of that user.
%% Returns the User Object.
read_user(UserId) ->
  F = fun() ->
    [User] = mnesia:read(tweet_user, UserId, write),
%%        Q = qlc:q([U || U <- mnesia:table(tweet_user), U#tweet_user.user_id == UserId]),
%%        qlc:e(Q)
%%        User = #user{user_id = UserId, login = '$1'},
%%        mnesia:select(user, [{User, [], ['$1']}])
      User
    end,
  {_, Result} = mnesia:transaction(F),

  Result.

%% Sign out the user.
%% flips the flag in the user.
signout_user(UserId) ->
  F = fun() ->
    [User] = mnesia:read(tweet_user, UserId, write),
    CurrentPid = User#tweet_user.current_pid,
    NewUser = User#tweet_user{login = false, current_pid = null},
    mnesia:write(NewUser),
    CurrentPid
  end,
%%  io:format("User [~p] Signed out", [UserId]),
  {atomic, CurrentPid} = mnesia:transaction(F),
  CurrentPid.

%% Check if the user is live
check_user_live(UserId) ->
  F = fun () ->

    [{tweet_user, _, IsAlive, CurrentPid}] = mnesia:read(tweet_user, UserId, write),

    {IsAlive, CurrentPid}
  end,
  {_, Result} = mnesia:transaction(F),



  Result.

%% Post a tweet
%% Posts a tweet with Mentions and HashTags
post_a_tweet(UserId, TweetString, Mentions, HashTags) ->
%%  io:format("Tweet to be posted [~p]", [TweetString]),

  TweetId = id_util:create_tweet_id(),

  %%% Create a Tweet entry
  Tweet = #tweets{
    poster = UserId,
    tweet_id = TweetId,
    content = TweetString,
    timestamp = erlang:system_time(millisecond)
  },

  F = fun() ->

    %% Insert tweet.
    mnesia:write(Tweet),

    %%% Handle Mentions part of this tweet one by one.
    lists:foreach(
      fun(Men) ->
        TweetMention = #tweet_mensions{
          tweet_id = TweetId,
          mension = Men
        },

        mnesia:write(TweetMention)

      end,
      Mentions
    ),

    %% Handle Hashtag part of the tweet one by one.
    lists:foreach(
      fun(Tag) ->
        HashTag = #tweet_hash_tag{
          tweet_id = TweetId,
          hash_tag = Tag
        },

        mnesia:write(HashTag)
      end,
      HashTags
    )
      end,

  mnesia:transaction(F).

%% Retweet a tweet with id TweetId
%% UserId -> The user with this id retweets.
retweet(UserId, TweetId) ->

  F = fun() ->

    %% Create a retweet
    Retweet = #retweet{
      user_id = UserId,
      tweet_id = TweetId,
      timestamp = erlang:system_time(millisecond)
    },
    mnesia:write(Retweet)
      end,

  mnesia:transaction(F),

  TweetId.

%%% Subscribe to the user with id -> UserId
%%% Subscriber -> the user whos subscribes to the events of user UserId
subscribe(UserId, Subscriber) ->

  F = fun() ->

    %% Create a Subscriber entry
    Subscriber = #subscribers{
      user_id = UserId,
      subscriber = Subscriber
    },

    mnesia:write(Subscriber)

  end,
  mnesia:transaction(F).

%%% Get the list of subscribers from of the user with id = UserID
%%% Get all the subscriber to a given user as list of UserId (Subscribers) returned.
get_subscribers(UserId) ->

  F = fun() ->

    Query = qlc:q([S#subscribers.subscriber || S <- mnesia:table(subscribers), S#subscribers.user_id == UserId]),

    Subscribers = qlc:e(Query),

    Subscribers

  end,

  {_, Result} = mnesia:transaction(F),

  Result.


%% User ID -> Id of the user whose feed is updated with the event
%% Tweet ID -> Tweet corresponding to the event
%% FeedType -> Type of the user feed, Why its part of someones feed
add_feed_to_user(UserId, TweetId, FeedType) ->
  add_feed_to_user_status(UserId, TweetId, FeedType, false).

add_feed_to_user_status(UserId, TweetId, FeedType, Viewed) ->

  F = fun() ->

    UserFeed = #user_feed{
      user_id = UserId,
      tweet_id = TweetId,
      type = FeedType,
      viewed = Viewed
    },

    mnesia:write(UserFeed)

  end,

  mnesia:transaction(F).

%% UserId -> if of the user whose feeds need to be fetched
%% Returns the list of TweetIds. Tweets need to fetched from tweets.
get_feeds_for_user(UserId) ->
  F = fun() ->

    Query = qlc:q([UF#user_feed.tweet_id || UF <- mnesia:table(user_feed), UF#user_feed.user_id == UserId]),

    UserFeedTweets = qlc:e(Query),

    UserFeedTweets

  end,

  {_, Result} = mnesia:transaction(F),

  Result.


%% Get tweets
get_tweets(TweetIds) ->
  F = fun() ->

    Query = qlc:q([{T#tweets.content, T#tweets.tweet_id} || T <- mnesia:table(tweets), lists:member(T#tweets.tweet_id, TweetIds)]),

    Tweets = qlc:e(Query),

    Tweets

  end,

  {_, Result} = mnesia:transaction(F),

  Result.

get_tweet_ids_hashtag(HashTag) ->
  F = fun () ->

    Query = qlc:q([TH#tweet_hash_tag.tweet_id || TH <- mnesia:table(tweet_hash_tag), TH#tweet_hash_tag.hash_tag == HashTag]),

    TweetIds = qlc:e(Query),

    TweetIds
  end,

  {_, Result} = mnesia:transaction(F),

  Result.

get_tweet_ids_by_user(UserId) ->
  F = fun () ->
    Query = qlc:q([T#tweets.tweet_id || T <- mnesia:table(tweets), T#tweets.poster == UserId]),

    TweetIds = qlc:e(Query),

    TweetIds
  end,

  {_, Result} = mnesia:transaction(F),

  Result.

get_tweets_from_tweet_feeds(TweetIds) ->

  Tweets = get_tweets(TweetIds),

  case Tweets of
    [] ->
      NewTweets = general_util:create_random_tweet(
            rand:uniform(20),
            []
          ),
%%      io:format("Tweet Returned is [~p]", [NewTweets]),
      NewTweets;
    _ ->
      Tweets
  end.










