main = print (primesTo 100000)

primesTo :: Int -> [Int]
primesTo n =
  let
    all = take (n-1) (iterate (+1) 2)
  in
    sieve [] all

sieve :: [Int] -> [Int] -> [Int]
sieve known [] = known
sieve known (n:ns) =
  let
    remaining = filter (\x -> (mod x n) > 0) ns
  in
    sieve (known ++ [n]) remaining
