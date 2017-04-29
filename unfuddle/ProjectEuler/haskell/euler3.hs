main = print (getPrimeFactors 13195)

getPrimeFactors :: Int -> [Int]
getPrimeFactors n =
  let primes = primesTo (floor (sqrt n))
  in factorHelper primes n

primesTo :: Int -> [Int]  
primesTo n = [1, 2, 3]

factorHelper :: [Int] -> Int -> [Int]
factorHelper list n = list