-module(get_put_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

init_per_testcase(_TestCase, Config) ->
  P = erlang:open_port({spawn, "../../priv/rockse default.db rockse_default.log"}, [{packet, 2}, binary]),
  [{port, P} | Config].

end_per_testcase(_TestCase, Config) ->
  P = ?config(port, Config),
  close(P),
  ok.

all() ->
  [put_info, get_info].

put_info(Config) ->
  P = ?config(port, Config),
  put(P, "hello", "world"),
  put(P, "end", "clause"),
  put(P, "kitty", "pyrde"),
  ok.

get_info(Config) ->
  P = ?config(port, Config),
  "clause" = get(P, "end"),
  "pyrde" = get(P, "kitty"),
  "world" = get(P, "hello"),
  ok.

close(P) ->
  erlang:port_command(P, term_to_binary(close)),
  receive
    {P, {data, Reply}} ->
      binary_to_term(Reply)
  end.

put(P, Key, Value) ->
  erlang:port_command(P, term_to_binary({put, Key, Value})),
  receive
    {P, {data, Reply}} ->
      binary_to_term(Reply)
  end.

get(P, Key) ->
  erlang:port_command(P, term_to_binary({get, Key})),
  receive
    {P, {data, Reply}} ->
      {ok, get, V} = binary_to_term(Reply),
      V
  end.
