-module(mr_util).
-export([list_reader/1, count_mapper/1, shuffle/1]).

list_reader(Data) ->
  receive
    {read, Caller} ->
      case Data of
        []    -> Caller ! eod, list_reader([]);
        [H|T] -> Caller ! H,   list_reader(T)
      end;
    close -> normal
  end.
  
count_mapper(X) -> {X, 1}.

shuffle(Data) -> do_shuffle(Data, []).

do_shuffle([], TL) -> lists:keysort(1, TL);
do_shuffle([{K,V}|T], TL) ->
  Told = lists:keysearch(K, 1, TL),
  Tnew = case Told of
    {value, {K, L}} -> {K, [V|L]};
    false           -> {K, [V]}
  end,
  do_shuffle(T, lists:keystore(K, 1, TL, Tnew)).
