%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 3:09 pm
%%%-------------------------------------------------------------------
-module(feeds).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_feed_request/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_feed_request}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_feed_request}],
    Req, State}.

handle_feed_request(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  UserId = proplists:get_value(<<"user">>, PostVals),

  %% Handle it as a live feed
  TweetIds = db_util:get_feeds_for_user(UserId),

  Tweets = db_util:get_tweets_from_tweet_feeds(TweetIds),

  %% Binary output echoed.
  Body = jsone:decode(#{<<"data">> => Tweets}),

  %% Return the json response
  {Body, Req2, State}.
