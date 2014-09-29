-module(rockse_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
  application:set_env(rockse, rockse_dir, "../../priv/rockse"),
  application:start(rockse),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

all() -> 
  [add_table, add_users, get_tom, delete_tom, get_tom_again].

add_table(_Config) ->
  [] = rockse:tables(),
  rockse:add_table(user),
  [user] = rockse:tables().

add_users(_Config) ->
  rockse:put(user, "jill", "jones"),
  rockse:put(user, "tom", "thumb"),
  rockse:put(user, "peter", "[paul, john, james, marcus, lukas]").

get_tom(_Config) ->
  {ok, "thumb"} = rockse:get(user, "tom").

delete_tom(_Config) ->
  deleteok = rockse:delete(user, "tom").

get_tom_again(_Config) ->
  {error, notfound} = rockse:get(user, "tom").
