-module(mr).
-export([run/3, test/0]).

test() -> run(fun(X) -> [{X, 1}] end,
              fun({K, L}) -> {K, length(L)} end,
              [1, 2, 4, 2, 3, 1, 1, 4, 3, 4, 3, 2]).

run(M, R, Input) ->
  reduce(R, shuffle(map(M, Input))).

map(F, Data)    -> lists:flatmap(F, Data).
shuffle(Data)   -> do_shuffle(Data, []).
reduce(R, Data) -> lists:map(R, Data).

do_shuffle([], TL) -> TL;
do_shuffle([{K,V}|R], TL) ->
  Told = lists:keysearch(K, 1, TL),
  Tnew = case Told of
    {value, {K, L}} -> {K, [V|L]};
    false           -> {K, [V]}
  end,
  do_shuffle(R, lists:keystore(K, 1, TL, Tnew)).
