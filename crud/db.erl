%%%-------------------------------------------------------------------
-module(db).
-author("kreastone").

-behaviour(gen_server).

%% API
-export([start/0]).
-export([new/1, create/2, read/2, update/2, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(db_state, {
  db_map :: map()
}).
-type person() :: {Key :: integer(), Username :: string(), City :: string()}.

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create new storage
-spec(new(DbName :: string()) ->
  {ok, Pid :: pid()} | {error, Reason :: term()}).
new(DbName) when
    (is_list(DbName) orelse is_binary(DbName)) ->
  gen_server:call(?SERVER, {new, DbName});
new(_) ->
  {error, badarg}.

%% @doc Create a new entry in the storage
-spec(create(Record :: person(), DbName :: string()) ->
  {ok, Record :: person()} | {error, Reason :: term()}).
create({Key, Username, City} = Record, DbName) when
    (is_list(DbName) orelse is_binary(DbName)) andalso
    (is_list(Username) orelse is_binary(Username)) andalso
    (is_list(City) orelse is_binary(City)) andalso
    is_integer(Key) ->
  gen_server:call(?SERVER, {create, Record, DbName});
create(_Record, _DbName) ->
  {error, badarg}.

%% @doc Reading entry from storage
-spec(read(Key :: integer(), DbName :: string()) ->
  {ok, Record :: person()} | {error, Reason :: term()}).
read(Key, DbName) when
    (is_list(DbName) orelse is_binary(DbName)) andalso
    is_integer(Key) ->
  gen_server:call(?SERVER, {read, Key, DbName});
read(_Key, _DbName) ->
  {error, badarg}.

%% @doc Updating entry from storage
-spec(update(Record :: person(), DbName :: string()) ->
  {ok, Record :: person()} | {error, Reason :: term()}).
update({Key, Username, City} = Record, DbName) when
    (is_list(DbName) orelse is_binary(DbName)) andalso
    (is_list(Username) orelse is_binary(Username)) andalso
    (is_list(City) orelse is_binary(City)) andalso
    is_integer(Key) ->
  gen_server:call(?SERVER, {update, Record, DbName});
update(_Key, _DbName) ->
  {error, badarg}.

%% @doc Deleting entry from storage
-spec(delete(Key :: integer(), DbName :: string()) ->
  {ok, Record :: person()} | {error, Reason :: term()}).
delete(Key, DbName) when
    (is_list(DbName) orelse is_binary(DbName)) andalso
    is_integer(Key) ->
  gen_server:call(?SERVER, {delete, Key, DbName});
delete(_Key, _DbName) ->
  {error, badarg}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #db_state{}} | {ok, State :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #db_state{db_map = maps:new()}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #db_state{}) ->
  {reply, Reply :: term(), NewState :: #db_state{}} |
  {reply, Reply :: term(), NewState :: #db_state{}, timeout() | hibernate} |
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #db_state{}} |
  {stop, Reason :: term(), NewState :: #db_state{}}).

handle_call({new, DbName}, _From, #db_state{db_map = DbMap} = State) ->
  case maps:find(DbName, DbMap) of
    {ok, _EtsTable} ->
      {reply, {error, bd_already_exists}, State};
    _ ->
      Ets = ets:new(my_bd, []),
      NewMap = maps:put(DbName, Ets, DbMap),
      NewState = State#db_state{db_map = NewMap},
      {reply, ok, NewState}
  end;

handle_call({create, {Key, _Username, _City} = Record, DbName}, _From, #db_state{db_map = DbMap} = State) ->
  Reply =
    case maps:find(DbName, DbMap) of
      {ok, Ets} ->
        case ets:lookup(Ets, Key) of
          [] ->
            ets:insert(Ets, Record),
            {ok, Record};
          _ -> {error, key_already_exists}
        end;
      _ -> {error, bd_not_found}
    end,
  {reply, Reply, State};

handle_call({read, Key, DbName}, _From, #db_state{db_map = DbMap} = State) ->
  Reply =
    case maps:find(DbName, DbMap) of
      {ok, Ets} ->
        case ets:lookup(Ets, Key) of
          [Record] -> {ok, Record};
          _ -> {error, key_not_found}
        end;
      _ -> {error, bd_not_found}
    end,
  {reply, Reply, State};

handle_call({update, {Key, _Username, _City} = Record, DbName}, _From, #db_state{db_map = DbMap} = State) ->
  Reply =
    case maps:find(DbName, DbMap) of
      {ok, Ets} ->
        case ets:lookup(Ets, Key) of
          [_Record] ->
            ets:insert(Ets, Record),
            {ok, Record};
          _ -> {error, key_not_found}
        end;
      _ -> {error, bd_not_found}
    end,
  {reply, Reply, State};

handle_call({delete, Key, DbName}, _From, #db_state{db_map = DbMap} = State) ->
  Reply =
    case maps:find(DbName, DbMap) of
      {ok, Ets} ->
        case ets:lookup(Ets, Key) of
          [_Record] ->
            ets:delete(Ets, Key),
            ok;
          _ -> {error, key_not_found}
        end;
      _ -> {error, bd_not_found}
    end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #db_state{}) ->
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #db_state{}}).
handle_cast(_Request, State = #db_state{}) ->
  {noreply, State}.
-spec(handle_info(Info :: timeout() | term(), State :: #db_state{}) ->
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #db_state{}}).
handle_info(_Info, State = #db_state{}) ->
  {noreply, State}.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #db_state{}) -> term()).
terminate(_Reason, _State = #db_state{}) ->
  ok.
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #db_state{},
    Extra :: term()) ->
  {ok, NewState :: #db_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #db_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================