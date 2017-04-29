import Control.Parallel

main = a `par` b `pseq` print (a + b)
  where a = fac 1000
        b = fib 1000

fac 0 = 1
fac n = n * fac (n - 1)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
