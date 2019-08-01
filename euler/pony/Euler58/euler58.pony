use "collections"
use "pdblib"

type U is U32

actor Main
  new create(env: Env) =>
    let primes = Primes
    var count: U = 0
    var n: U = 1
    var i: U = 0
    var ratio: F64 = -1
    repeat
      i = i + 2
      for j in Range[U](0, 4) do
        n = n + i
        if primes.isprime(n) then count = count + 1 end
      end
      let total: U = (2 * i) + 1
      ratio = count.f64() / total.f64()
    until ratio < 0.1 end
    Util.printarray[F64](env.err, [(i + 1).f64(); ratio])
