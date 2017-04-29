module Main where

import Control.Concurrent.Chan
import Control.Concurrent

-- Fork some computation processes, print their results
main = do
    primes <- run primes
    fibs   <- run fibonacci
    mapM print $ take 100 $ zip primes fibs

  -- fork a process, return any messages it produces as a list
  where
    run f = do
        c <- newChan
        l <- getChanContents c
        forkIO (writeList2Chan c f)
        return l

-- A function to compute primes
primes = sieve [2..]
    where
       sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- A function to compute fibonacci numbers
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
