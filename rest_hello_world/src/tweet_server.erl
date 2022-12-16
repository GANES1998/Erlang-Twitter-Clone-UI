%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2022 2:58 pm
%%%-------------------------------------------------------------------
-module(tweet_server).
-author("ganesonravichandran").

%% Record to store and maintain server stats.
-record(server_stats, {
  time = 0,
  no_sign_in = 0,
  tweets = 0,
  add_feeds = 0,
  live_feeds_req = 0,
  live_feeds_res = 0,
  bg_feeds_req = 0,
  bg_feeds_res = 0,
  search_req_cnt = 0,
  search_res_cnt = 0,
  subscribe_reqs = 0,
  retweets = 0
}).

%% API
-export([main/1]).


listen_for_events(Stats) ->
%%  io:format("~p~n", [Stats]),

  receive
    {signup, Pid} ->

      %% Insert to Db
      UserId = db_util:signup_user(Pid),

      %% send back the user id
      Pid ! {signup_success, UserId},

      %% Reiterate and lister again
      listen_for_events(Stats#server_stats{
        no_sign_in = Stats#server_stats.no_sign_in + 1
      });

    {signout, UserId} ->

      %% Update signout
      CurrentPid = db_util:signout_user(UserId),

%%      io:format("Server obtained Current Pid ~p~n", [CurrentPid]),

      CurrentPid ! {signout_success},

      listen_for_events(
        Stats
      );

    {tweet, {UserId, Tweet, Mentions, Hashtags, CallerPid}} ->

      %% Insert into Db.
      TweetId = db_util:post_a_tweet(UserId, Tweet, Mentions, Hashtags),

      %% Add to subscribers feed.
      Subscribers = db_util:get_subscribers(UserId),

      %% Add the tweet to the feed of subscribers
      lists:foreach(
        fun(SubUserId) ->
          %% publish an event to same server of the feed
          ?MODULE ! {feed, {SubUserId, TweetId, tweet}}
        end,
        Subscribers
      ),

      %% Add tweet to the feed of persons mentioned
      lists:foreach(
        fun(MentionUserId) ->
          %% publish an even to add to the feed of the user
          ?MODULE ! {feed, {MentionUserId, TweetId, mension}}
        end,
        Mentions
      ),

      CallerPid ! {tweet_success},

      listen_for_events(
        Stats#server_stats{
          tweets = Stats#server_stats.tweets + 1
        }
      );

    %% Get Feeds associated with a user
    {get_feed, {UserId, IsLive, CallerPid}} ->
      case IsLive of
        true ->
          %% Handle it as a live feed
          TweetIds = db_util:get_feeds_for_user(UserId),

          Tweets = db_util:get_tweets_from_tweet_feeds(TweetIds),

          {_, _, _, UserPid} = db_util:read_user(UserId),

          case UserPid of
            null -> listen_for_events(Stats);
            _ ->
              UserPid ! {feed_tweets, {Tweets}},

              listen_for_events(Stats#server_stats{
                live_feeds_req = Stats#server_stats.live_feeds_req + 1,
                live_feeds_res = Stats#server_stats.live_feeds_res + length(Tweets)
              })
          end;
        false ->
          TweetIds = db_util:get_feeds_for_user(UserId),

          Tweets = db_util:get_tweets_from_tweet_feeds(TweetIds),

          case CallerPid of
            null -> listen_for_events(Stats);
            _ ->
%%              io:format("Non Null Caller Pid Called [~p]~n", [CallerPid]),

              CallerPid ! {feed_tweets, {Tweets}},

              listen_for_events(Stats#server_stats{
                bg_feeds_req = Stats#server_stats.bg_feeds_req + 1,
                bg_feeds_res = Stats#server_stats.bg_feeds_res + length(Tweets)
              })
          end
      end;

    %% Handle a new feed added to the system.
    {feed, {UserId, TweetId, Type}} ->

      case db_util:check_user_live(UserId) of
        {true, _CurrentPid} ->
          %% Since the user is online, (Alive), we are adding the feed to true
          db_util:add_feed_to_user_status(UserId, TweetId, Type, true),

          tweet_server ! {get_feed, {UserId, true, 0}},

          listen_for_events(Stats#server_stats{
            add_feeds = Stats#server_stats.add_feeds + 1
          });

        {false, _} ->

          %% Since the user is online, (Alive), we are adding the feed to true
          db_util:add_feed_to_user(UserId, TweetId, Type),

          listen_for_events(Stats#server_stats{
            add_feeds = Stats#server_stats.add_feeds + 1
          })
      end;

    %% Search Tweet
    {search_hashtag, {HashTag, CallerPid}} ->
      TweetIds = db_util:get_tweet_ids_hashtag(HashTag),

      Tweets = db_util:get_tweets(TweetIds),

      CallerPid ! {search_result, Tweets},

      listen_for_events(Stats#server_stats{
        search_req_cnt = Stats#server_stats.search_req_cnt + 1,
%%        search_res_cnt = length(Tweets) + Stats#server_stats.search_res_cnt
        search_res_cnt =  Stats#server_stats.search_res_cnt + server_utils:get_tweet_count_from_tweets(Tweets)
      });

    %% Subscribe to another user, so that you get tweets of them.
    {subscribe, {UserId, CallerUserId, CallerPid}} ->

      db_util:subscribe(UserId, CallerUserId),

      CallerPid ! {subscribe_success},

      listen_for_events(Stats#server_stats{
        subscribe_reqs = Stats#server_stats.subscribe_reqs + 1
      });

    %% Retweet - another tweet.
    {retweet, {UserId, TweetId, CallerPid}} ->

      db_util:retweet(UserId, TweetId),

      CallerPid ! {retweet_success},

      listen_for_events(Stats#server_stats{
        retweets = Stats#server_stats.retweets + 1
      });

    %% Get tweets by the user
    {get_tweets, {UserId, CallerPid}} ->

      TweetIds = db_util:get_tweet_ids_by_user(UserId),

      CallerPid ! {tweets_by_user, TweetIds},

      listen_for_events(Stats);

    {converged} ->
      Stats

    after 5000 ->
          io:format("Did not receive any tweets in the last [~p] ms. So terminating server....", [5000]),
          Stats

  end.

main(N) ->
  %% Register current process
  register(?MODULE, self()),

  %% Init Mnesia ORM DB
  db_connection:main(),

  %% Go to main loop to listen for inputs
  Stats = #server_stats{
    no_sign_in = 0,
    tweets = 0,
    add_feeds = 0,
    live_feeds_req = 0,
    live_feeds_res = 0,
    bg_feeds_req = 0,
    bg_feeds_res = 0,
    search_req_cnt = 0,
    search_res_cnt = 0,
    subscribe_reqs = 0,
    retweets = 0
  },

  spawn_link(node(), twitter_client_simulator, main, [N]),

  Start = os:system_time(millisecond),

  %% Go to live main loop
  UpdatedStats = listen_for_events(Stats),

  End = os:system_time(millisecond),

  io:format("Server Teminated with Stats [ ~p ] ~n", [UpdatedStats#server_stats{time = End - Start}]).