%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 2:30 pm
%%%-------------------------------------------------------------------
-module(signup).
-author("ganesonravichandran").


-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_signup/2]).

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

handle_signup(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  Username = proplists:get_value(<<"username">>, PostVals),
  _Password = proplists:get_value(<<"password">>, PostVals),

  %% Insert to Db
  UserId = db_util:signup_user(Username),

  %% Binary output echoed.
  Body = <<"{\"data\": \"", UserId/binary, "\"}">>,

  %% Return the json response
  {Body, Req2, State}.
