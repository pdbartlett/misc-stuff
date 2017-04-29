# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143?

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

puts getPrimeFactors(13195).keys.sort.join(" ")
puts getPrimeFactors(600851475143).keys.max
