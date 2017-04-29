main = print (solve [ [2, 1, 7, 45], [5, 3, 1, 32], [1, 2, 3, 26] ])

solve :: Fractional a => [[a]] -> [a]
solve [[x, n]] = [ n / x ]
solve (h:t) = 
  let
    coeff = head h
    normalized = map (/ coeff) h
    simpler = map (simplify normalized) t
    otherSols = solve simpler
    adjustedRhs = (last h) - sum (zipWith (*) (init (tail h)) otherSols)
    solution = adjustedRhs / coeff
  in
    solution : otherSols
    
simplify :: Fractional a => [a] -> [a] -> [a]
simplify norm row = tail (zipWith (\ x y -> y - x * (head row)) norm row)
