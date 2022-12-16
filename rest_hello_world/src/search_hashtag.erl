%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 3:16 pm
%%%-------------------------------------------------------------------
-module(search_hashtag).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_search_request/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_search_request}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_search_request}],
    Req, State}.

handle_search_request(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the hashtag
  Hashtag = proplists:get_value(<<"hashtag">>, PostVals),

  TweetIds = db_util:get_tweet_ids_hashtag(Hashtag),

  Tweets = db_util:get_tweets(TweetIds),

  %% Binary output echoed.
  Body = jsone:encode(#{<<"data">> => Tweets}),

  %% Return the json response
  {Body, Req2, State}.
