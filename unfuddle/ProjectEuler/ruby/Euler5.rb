def primesTo(n)
  sieve = {}
  for i in 2..n do
    sieve[i] = true
  end
  for i in 2..(n/2).floor do
    next unless sieve[i]
    for j in 2..(n/i).floor do
      sieve[j*i] = false
    end
  end
  sieve.reject! { |k,v| not v }
  return sieve.keys
end

def getPrimeFactors(n)
  primeFactors = {}
  current = n
  for prime in primesTo(Math.sqrt(n)) do
    while current.remainder(prime) == 0 do
      current /= prime
      count = (primeFactors[prime] or 0)
      primeFactors[prime] = count + 1
    end
  end
  return primeFactors.empty? ? {n => 1} : primeFactors
end

def solveIt(n)
  allPrimeFactors = {}
  for i in 2..n do
    thisPrimeFactors = getPrimeFactors(i)
    for factor in thisPrimeFactors.keys do
      newCount = thisPrimeFactors[factor]
      prevCount = (allPrimeFactors[factor] or 0)
      allPrimeFactors[factor] = [newCount, prevCount].max
    end
  end
  product = 1
  allPrimeFactors.each { |k,v| product *= k**v }
  return product 
end

puts solveIt(10)
puts solveIt(20)