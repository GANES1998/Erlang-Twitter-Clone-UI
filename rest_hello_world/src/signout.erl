%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 2:44 pm
%%%-------------------------------------------------------------------
-module(signout).
-author("ganesonravichandran").

-export([allowed_methods/2, init/2, content_types_provided/2, content_types_accepted/2, handle_signout/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_signout}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application/json">>, <<"application/x-www-form-urlencoded">>, '*'}, handle_signout}],
    Req, State}.

handle_signout(Req, State) ->
  %%  Read the url encoded body
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),

  %% Get the echo from input
  Username = proplists:get_value(<<"username">>, PostVals),

  %% Insert to Db
  UserId = db_util:signout_user(Username),

  %% Binary output echoed.
  Body = <<"{\"data\": \"", UserId/binary, "\"}">>,

  %% Return the json response
  {Body, Req2, State}.