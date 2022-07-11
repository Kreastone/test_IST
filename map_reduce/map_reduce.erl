%%%-------------------------------------------------------------------
-module(map_reduce).
-author("kreastone").

%% API
-export([start/1]).

-export([proccess_reduce_map/2, proccess_word_counter/2]).

-spec(start(ListFiles :: list()) ->
  {ok, Map :: map()} | {error, Reason :: term()}).
start([]) -> {ok, #{}};
start(Files) ->
  PidReduce = spawn(?MODULE, proccess_reduce_map, [length(Files), self()]),
  start(Files, PidReduce).

start([], _PidReduce) ->
  receive
    Res -> Res
  end;
start([File|Files], PidReduce) ->
  spawn(?MODULE, proccess_word_counter, [File, PidReduce]),
  start(Files, PidReduce).

proccess_reduce_map(Count, From) ->
  loop_reduce_map(Count, From, maps:new()).
loop_reduce_map(0, MainPid, Res) ->
  MainPid ! {ok, Res};
loop_reduce_map(Count, MainPid, ResMap) ->
  receive
    {ok, GetMap} -> loop_reduce_map(Count - 1, MainPid, merge_map(ResMap, GetMap));
    Error -> MainPid ! Error
  end.
merge_map(Map1, Map2) ->
  maps:merge_with(fun(_Key, Value1, Value2) -> Value1 + Value2 end, Map1, Map2).


%%=======================================
%% UTF-8
-define(ALowerCase, 97).
-define(ZLowerCase, 122).
-define(ACapitalCase, 65).
-define(ZCapitalCase, 90).
%% Если встретим неразрешенный символ, то будем считать что это уже не текст
-define(ListAllowedCharacters, " ,.?!:;'\"\n\t\r1234567890[]()@#$%^&*~-+<>=/_").
-define(LenReadFile, 128).

-record(state, {
  is_word = false, %% true : false
  buf_word = <<"">>,
  result = #{}
}).
proccess_word_counter(FileName, PidReduce) ->
  Send =
    case file:open(FileName, [read, raw, binary]) of
      {ok, Fd} -> file_reader(Fd, FileName);
      Error -> {error, [{filename, FileName},Error]}
    end,
  PidReduce ! Send.

file_reader(Fd, FileName) ->
  file_reader(Fd, FileName, #state{}).
file_reader(Fd, FileName, State) ->
  case file:read(Fd, ?LenReadFile) of
    eof when State#state.is_word ->
      file:close(Fd),
      {ok, update_map(State#state.result, State#state.buf_word)};
    eof ->
      file:close(Fd),
      {ok, State#state.result};
    {ok, Data} ->
      case handle_data(Data, State) of
        {ok, NewState} -> file_reader(Fd, FileName, NewState);
        {error, Error} ->
          file:close(Fd),
          {error, [{filename, FileName},Error]}
      end
  end.

%% Тут есть сложность. Слова AND, And или and считаются разными словами, хотя это одно и то же слово.
%% Эти случаи не обрабатываются
handle_data(<<"">>, State) -> {ok, State};
handle_data(<<Char:8, Data/bitstring>>, State) when
  (Char >= ?ALowerCase andalso Char =< ?ZLowerCase) orelse
  (Char >= ?ACapitalCase andalso Char =< ?ZCapitalCase) ->
  handle_data(Data, State#state{
    is_word = true,
    buf_word = <<(State#state.buf_word)/binary, Char:8>>
  });
handle_data(<<Char:8, Data/bitstring>>, State) ->
  case (lists:member(Char, ?ListAllowedCharacters)) of
    true when State#state.is_word ->
      handle_data(Data, #state{
        is_word = false,
        buf_word = <<"">>,
        result = update_map(State#state.result, State#state.buf_word)
      });
    true -> handle_data(Data, State);
    false ->
      {error, [{undefined_character, <<Char>>}]}
  end.

update_map(Map, Word) ->
  case maps:get(Word, Map, []) of
    [] -> maps:put(Word, 1, Map);
    Value -> maps:update(Word, Value + 1, Map)
  end.