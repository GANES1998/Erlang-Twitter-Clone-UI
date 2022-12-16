%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_h, []},
			{"/signup", signup, []},
			{"/signout", signout, []},
			{"/tweets", tweets, []},
			{"/feeds", feeds, []},
			{"/searchhashtags", search_hashtags, []},
			{"/subscribe", subscribe_handler, []},
			{"/retweet", retweet, []},
			{"/getmentions", getmentions, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	rest_hello_world_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).
