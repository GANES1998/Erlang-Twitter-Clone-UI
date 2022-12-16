%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 4:16 pm
%%%-------------------------------------------------------------------
-module(getmentions).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_get_mentions/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_get_mentions}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_get_mentions}],
    Req, State}.

handle_get_mentions(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  UserId = proplists:get_value(<<"userId">>, PostVals),

  %% Insert to Db
  Tweets = db_util:get_tweets(UserId),

  %% Binary output echoed.
  Body = <<"{\"data\": \"", Tweets/binary, "\"}">>,

  %% Return the json response
  {Body, Req2, State}.
