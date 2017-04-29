-module(pdb).
-export([fac/1, primesTo/1]).

fac(1) -> 1;
fac(N) -> N * fac(N - 1).

primesTo(N) -> processSieve(N, makeSieve(N, []), []).

makeSieve(1, List) -> List;
makeSieve(X, List) -> makeSieve(X - 1, [{X, true} | List]).

processSieve(_, [], Primes) -> lists:reverse(Primes);
processSieve(N, [Head | Rest], Primes) ->
  Max = math:sqrt(N),
  case Head of
    {X, _} when X > Max -> copyRemaining([Head | Rest], Primes);
    {_, false} -> processSieve(N, Rest, Primes);
    {X, true} -> processSieve(N, markMultiples(X, Rest, []), [X | Primes])
  end.

markMultiples(_, [], Done) -> Done;
markMultiples(X, [Head | Rest], Done) ->
  case Head of
    {_, false} -> markMultiples(X, Rest, Done ++ [Head]);
    {N, true} when N rem X == 0 -> markMultiples(X, Rest, Done ++ [{N, false}]);
    {N, true} -> markMultiples(X, Rest, Done ++ [{N, true}])
  end.

copyRemaining([], Primes) -> lists:reverse(Primes);
copyRemaining([{X, B} | Rest], Primes) when B -> copyRemaining(Rest, [X | Primes]);
copyRemaining([_ | Rest], Primes) -> copyRemaining(Rest, Primes).
