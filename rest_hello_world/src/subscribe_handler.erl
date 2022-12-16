%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 4:01 pm
%%%-------------------------------------------------------------------
-module(subscribe_handler).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_subscription_request/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_signup}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_signup}],
    Req, State}.

handle_subscription_request(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  UserId = proplists:get_value(<<"user">>, PostVals),
  Subscription = proplists:get_value(<<"subscription">>, PostVals),

  db_util:subscribe(UserId, Subscription),

  %% Binary output echoed.
  Body = jsone:decode(#{<<"data">> => UserId}),

  %% Return the json response
  {Body, Req2, State}.
