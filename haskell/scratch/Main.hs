module Main (main) where
fac n = if n < 2 then 1 else n * fac (n - 1)
main = print (fac 6)
