-module(mr2).
-export([run/7, test_reader/1, simple_map_manager/4, shuffle_reducer/4,
         count_mapper/1, length_reducer/1,
         test/0, simple_test/0, reader_test/0]).

test() -> lists:all(fun(T) -> apply(mr2, T, []) end,
    [ simple_test, reader_test ]).

test_data() -> [1, 1, 2, 3].

simple_test() ->
  run(mr2, test_reader, [test_data()], mr2, count_mapper, mr, length_reducer) == [{1,2},{2,1},{3,1}].

reader_test() ->
  RMP = spawn(mr2, test_reader, [test_data()]),
  Data = reader_test_loop(RMP, []),
  Data == test_data().

reader_test_loop(RMP, Acc) ->
  RMP ! {read, self()},
  receive
    eod -> RMP ! done, lists:reverse(Acc);
    X -> reader_test_loop(RMP, [X|Acc])
  end.

run(RMM, RMF, RMA, MM, MF, SM, SF) ->
  RMP = spawn(RMM, RMF, RMA),
  SRP = spawn(mr2, shuffle_reducer, [SM, SF, self(), []]),
  spawn(mr2, simple_map_manager, [MM, MF, RMP, self()]),
  run_loop(SRP).
  
run_loop(SRP) ->
  receive
    {K,V} -> SRP ! {K,V}, run_loop(SRP);
    eod   -> SRP ! reduce;
    Res   -> Res
  end.

test_reader(Data) ->
 receive
    {read, Caller} ->
      case Data of
        []    -> Caller ! eod, test_reader([]);
        [H|T] -> Caller ! H,   test_reader(T)
      end;
    done -> normal
  end.

simple_map_manager(MM, MF, RMP, Main) ->
  RMP ! {read, self()},
  receive
    eod -> Main ! eod, RMP ! done, normal;
    X   -> Main ! apply(MM, MF, [X]), simple_map_manager(MM, MF, RMP, Main)
  end.
  
shuffle_reducer(RM, RF, Main, Acc) ->
  receive
    {K,V}  -> shuffle_reducer(RM, RF, Main, [{K,V}|Acc]);
    reduce -> Main ! lists:map(fun(Tup) -> apply(RM, RF, [Tup]) end, Acc), normal
  end.

count_mapper(X) -> {X, 1}.

length_reducer({K, L}) -> {K, length(L)}.