-module(rockse).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([add_table/1, tables/0]).
-export([put/3, get/2, delete/2]).

%% del_table/1, put/3 (table,key,value), get/2 (table,key), get/1 (key)

%%
%% Create the dbase directory if needed and add existing tables
%% to the supervisor.
%%
start(_Type, _Args) ->
  Dir = application:get_env(?MODULE, dir, "Rocksdb." ++ atom_to_list(node())),
  application:set_env(?MODULE, dir, Dir),    %% set location to tables
  case application:get_env(?MODULE, rockse_dir, undefined) of
    undefined ->
      application:set_env(?MODULE, rockse_dir, "./priv/rockse");  %% set path to default rockse location
    _Path ->
      ok   %% rockse_dir was previously set for us
  end,
  TableList = create_db_dir(Dir),
  Result = rockse_sup:start_link(),
  application:set_env(?MODULE, tables, [list_to_atom(T) || T <- TableList]),
  lists:foreach(fun(X) ->
		    rockse_sup:add_table(X)
		end, TableList),
  Result.

tables() ->
  application:get_env(?MODULE, tables, []).

stop(_State) ->
	ok.

put(Table, Key, Value) ->
  rockse_sup:put(Table, Key, Value).

get(Table, Key) ->
  rockse_sup:get(Table, Key).

delete(Table, Key) ->
  rockse_sup:delete(Table, Key).

add_table(TabName) ->
  {ok, {_Tab, Pid}} = rockse_sup:add_table(TabName),
  NewTables = [TabName | ?MODULE:tables()],
  application:set_env(?MODULE, tables, NewTables),
  {ok, Pid}.

create_db_dir(Path) ->
  case file:make_dir(Path) of
    ok ->
      [];
    {error, eexist} ->
      get_list_of_tables(Path);
    {error, Reason} ->
      exit(Reason)
  end.

get_list_of_tables(Path) ->
  case file:list_dir(Path) of
    {ok, Tables} ->
      Tables;
    {error, Reason} ->
      exit(Reason)
  end.
