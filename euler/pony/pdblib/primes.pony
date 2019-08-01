use "collections"

class Primes
  fun isprime(n: U32): Bool =>
    for i in Range[U32](2, n.f64().sqrt().u32() + 1) do
      if (n % i) == 0 then return false end
    end
    true
