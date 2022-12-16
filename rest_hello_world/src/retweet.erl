%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 4:06 pm
%%%-------------------------------------------------------------------
-module(retweet).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_retweet/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_retweet}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_retweet}], Req, State}.

handle_retweet(Req, State) ->
  %% Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  UserId = proplists:get_value(<<"user">>, PostVals),
  TweetId = proplists:get_value(<<"tweetId">>, PostVals),

  %% Read th
  db_util:retweet(UserId, TweetId),

  %% Binary output echoed.
  Body = jsone:encode(#{<<"data">> : <<"Retweeted Successfully">>}),

  %% Return the json response
  {Body, Req2, State}.
