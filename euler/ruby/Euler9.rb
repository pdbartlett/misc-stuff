def isPythagTriplet(a, b, c)
  return (c * c) == (a * a + b * b)
end

def solveIt(n)
  for i in 1..(n - 2)
    for j in i..(n - (i + 1))
      k = n - (i + j)
      return [i, j, k] if isPythagTriplet(i, j, k)
    end
  end
end

answer = solveIt(1000)
puts "Triplet is " + answer.join(" ")

product = 1
answer.each { |n| product *= n }
puts "Product is " + product.to_s