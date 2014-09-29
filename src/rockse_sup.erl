-module(rockse_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([add_table/1, put/3, get/2, delete/2]).

-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_table(Table) ->
  {ok, Dir} = application:get_env(rockse, dir),
  {ok, Pid} = supervisor:start_child(?MODULE, [Table, Dir]),
  add_table_aux(Pid, Table).

add_table_aux(Pid, Table) when is_atom(Table) ->
  add_table_aux(Pid, atom_to_list(Table));
add_table_aux(Pid, Table) when is_list(Table) ->
  %% yes being evil creating atoms dynamically
  %% in my defense it is bounded by the number of tables you have.
  Tab = list_to_atom(Table ++ "_rocksdb"),
  true = register(Tab, Pid),
  {ok, {Tab, Pid}}.

put(Table, Key, Value) ->
  Tab = list_to_existing_atom(atom_to_list(Table) ++ "_rocksdb"),
  gen_server:call(Tab, {put, Key, Value}).

get(Table, Key) ->
  Tab = list_to_existing_atom(atom_to_list(Table) ++ "_rocksdb"),
  gen_server:call(Tab, {get, Key}).

delete(Table, Key) ->
  Tab = list_to_existing_atom(atom_to_list(Table) ++ "_rocksdb"),
  gen_server:call(Tab, {delete, Key}).

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  RockseTable = {'rockse_tbl', {'rockse_tbl', start_link, []},
		 Restart, Shutdown, Type, ['rockse_tbl']},

  {ok, {SupFlags, [RockseTable]}}.
