def sumTo(n)
  return n * (n + 1) / 2
end
  
def squareOfSum(n)
  s = sumTo(n)
  return s * s
end

def sumOfSquares(n)
  total = 0
  (1..n).each { |n| total += (n * n) }
  return total
end

def solveIt(n)
  return (squareOfSum(n) - sumOfSquares(n)).abs
end

puts solveIt(10)
puts solveIt(100)
