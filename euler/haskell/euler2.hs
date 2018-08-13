main = print (solveIt 4000000)

solveIt n = sum (filter even (fibonacciTo n))

fibonacciTo max = fibHelper max [1, 1]

fibHelper :: Int -> [Int] -> [Int]
fibHelper max list = let
    (y, allButY) = pop list
    x = last allButY
    next = x + y
  in
    if (next < max) then (fibHelper max (list ++ [next])) else list
    
pop :: [a] -> (a, [a])
pop as = (last as, init as)