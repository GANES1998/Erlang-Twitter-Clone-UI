%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 2:58 pm
%%%-------------------------------------------------------------------
-module(tweets).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_post_tweet/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_post_tweet}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_post_tweet}],
    Req, State}.

handle_post_tweet(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  UserId = proplists:get_value(<<"user">>, PostVals),
  Tweet = proplists:get_value(<<"tweet">>, PostVals),
  Mentions = proplists:get_value(<<"mentions">>, PostVals),
  Hashtags = proplists:get_value(<<"hashtags">>, PostVals),

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

  %% Binary output echoed.
  Body = jsone:decode(#{<<"TweetId">> => TweetId, <<"UserId">> => UserId, <<"Tweet">> => Tweet}),

  %% Return the json response
  {Body, Req2, State}.
