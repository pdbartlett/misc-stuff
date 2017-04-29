-module(mr_test).
-export([test/0, reader_test/0, count_mapper_test/0, shuffle_test/0]).

test() -> lists:all(fun(T) -> apply(mr_test, T, []) end,
    [reader_test, count_mapper_test, shuffle_test]).

test_data() -> [1, 4, 2, 8, 1].

reader_test() ->
  RP = spawn(mr_util, list_reader, [test_data()]),
  reader_test_loop(RP, []) == test_data().

reader_test_loop(RP, Acc) ->
  RP ! {read, self()},
  receive
    eod -> RP ! close, lists:reverse(Acc);
    X   -> reader_test_loop(RP, [X|Acc])
  end.
  
count_mapper_test() ->
  Res = lists:map(fun mr_util:count_mapper/1, test_data()),
  % io:format("~w~n", [Res]),
  Res == [{1, 1}, {4, 1}, {2, 1}, {8, 1}, {1, 1}].
  
shuffle_test() ->
  Res = mr_util:shuffle(lists:map(fun mr_util:count_mapper/1, test_data())),
  % io:format("~w~n", [Res]),
  Res == [{1, [1, 1]}, {2, [1]}, {4, [1]}, {8, [1]}].