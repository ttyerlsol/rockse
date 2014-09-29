-module(rockse_tbl).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {table, path, port}).

-define(TIMEOUT, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Table, Dir) when is_atom(Table) ->
  start_link(atom_to_list(Table), Dir);
start_link(Table, Dir) when is_list(Table) ->
  DbDir = Dir ++ "/" ++ Table,
  file:make_dir(DbDir),
  gen_server:start_link(?MODULE, [Table, DbDir], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Table, Dir]) ->
  {ok, Port} = open(Dir, Table),
  error_logger:info_msg("Table ~p~n   in ~p~n   using ~p~n", [Table, Dir, Port]), 
  {ok, #state{table = Table, path = Dir, port = Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({put, Key, Value}, _From, #state{port = P} = State) ->
  Reply = put(P, Key, Value),
  error_logger:info_msg("~p --> put ~p ~p~n", [self(), Key, Value]),
  {reply, Reply, State};
handle_call({delete, Key}, _From, #state{port = P} = State) ->
  Reply = delete(P, Key),
  error_logger:info_msg("~p --> delete ~p~n", [self(), Key]),
  {reply, Reply, State};
handle_call({get, Key}, _From, #state{port = P} = State) ->
  Reply = get(P, Key),
  error_logger:info_msg("~p --> get ~p~n", [self(), Key]),
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{table = Table, path = Dir, port = Port}) ->
  bok = close(Port),
  error_logger:info_msg("Closing table ~p~n   in ~p~n   using ~p~n", [Table, Dir, Port]), 
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open(TablePath, Table) ->
  Log = TablePath ++ "/rockse_" ++ Table ++ ".log",
  Cmd = application:get_env(rockse, rockse_dir, "./priv/rockse") ++ "  " ++ TablePath ++ " " ++ Log,
  error_logger:info_msg("~p opening~n   ~p~n", [self(), Cmd]),
  P = erlang:open_port({spawn, Cmd}, [{packet, 2}, binary]),
  {ok, P}.

close(P) ->
  erlang:port_command(P, term_to_binary(close)),
  receive
    {P, {data, Reply}} ->
      binary_to_term(Reply)
  after ?TIMEOUT ->
      {error, timeout}
  end.

put(P, Key, Value) ->
  erlang:port_command(P, term_to_binary({put, Key, Value})),
  receive
    {P, {data, Reply}} ->
      binary_to_term(Reply)
  after ?TIMEOUT ->
      {error, timeout}
  end.

get(P, Key) ->
  erlang:port_command(P, term_to_binary({get, Key})),
  receive
    {P, {data, Reply}} ->
      case binary_to_term(Reply) of
	{ok, get, V} ->
	  {ok, V};
	Error ->  %% can only be {error, notfound}
	  Error
      end
  after ?TIMEOUT ->
      {error, timeout}
  end.

delete(P, Key) ->
  erlang:port_command(P, term_to_binary({delete, Key})),
  receive
    {P, {data, Reply}} ->
      binary_to_term(Reply)
  after ?TIMEOUT ->
      {error, timeout}
  end.
