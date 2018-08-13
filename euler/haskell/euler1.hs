main = print (solveIt 1000)

solveIt :: Int -> Int
solveIt n = foldl1 (+) (filter matches (takeWhile (< n) (enumFrom 1)))

matches :: Int -> Bool
matches n = ((mod n 3) == 0) || ((mod n 5) == 0)