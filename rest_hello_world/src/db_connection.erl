%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2022 9:17 am
%%%-------------------------------------------------------------------
-module(db_connection).
-author("ganesonravichandran").

%% API
-include_lib("stdlib/include/qlc.hrl").
-include("db_record.hrl").

-export([main/0]).

main() ->

  %% Start Mnesia DB.
  mnesia:create_schema([node()]),
  mnesia:start(),

  %% Create Tables based on the definitions. Like ORM
  mnesia:create_table(tweet_user, [{attributes, record_info(fields, tweet_user)}]),
  mnesia:create_table(tweets, [{attributes, record_info(fields, tweets)}]),
  mnesia:create_table(tweet_hash_tag, [{attributes, record_info(fields, tweet_hash_tag)}]),
  mnesia:create_table(tweet_mensions, [{attributes, record_info(fields, tweet_mensions)}]),
  mnesia:create_table(user_feed, [{attributes, record_info(fields, user_feed)}]),
  mnesia:create_table(subscribers, [{attributes, record_info(fields, subscribers)}]),
  mnesia:create_table(retweet, [{attributes, record_info(fields, retweet)}]),

%%  UesrId = db_util:signup_user(),
%%
%%  User = db_util:read_user(UesrId),
%%
%%  io:format("~p~n", [User]),
%%
%%  db_util:signout_user(UesrId),
%%
%%  SOUser = db_util:read_user(UesrId),
%%
%%  io:format("~p~n", [SOUser]),
%%
%%  Tweet = general_util:create_a_string(
%%    [
%%%%      {4, s, ""},
%%%%      {3, s, ""},
%%      {7, s, ""},
%%      {3, mention, "Ganeson"},
%%      {4, hashtag, "COP5734"},
%%      {4, s, ""}
%%    ]
%%  ),
%%
%%  db_util:post_a_tweet(UesrId, Tweet, ["Ganeson"], ["COP5724"]),


%%  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),

%%  application:stop(odbc),

%%  case application:start(odbc) of
%%    ok ->
%%      io:format("ODBC is started");
%%    {error, Reason} -> io:format("~p", [Reason])
%%  end,

%%  mysql:start_link(db_connection, "localhost", 1100, "ganeson", "password", "TWITTER_CLONE", true),
%%
%%  mysql:connect(db_connection, "localhost", undefined, "ganeson", "password", "TWITTER_CLONE"),
%%
%%  mysql:fetch(db_connection, <<"INSERT INTO USER VALUES"
%%                               "('ganeson', 1)">>),

  io:format("Mnesia Init Successfull~n").