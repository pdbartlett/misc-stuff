# First six primes are: 2, 3, 5, 7, 11. Find 10001st prime.

def extendBy(known, n)
  sieve = {}
  last = (known.last or 1) # so we start at 2
  firstCandidate = last + 1
  lastCandidate = last + n
  print "Extending by ", n, " to ", lastCandidate, "\n"
  for i in firstCandidate..lastCandidate do
    sieve[i] = true
  end
  for prime in known do
    firstMultiplier = (firstCandidate / prime).ceil
    lastMultiplier = (lastCandidate / prime).floor
    for i in firstMultiplier..lastMultiplier do
      sieve[prime * i] = false
    end
  end
  lastCandidateFactor = firstCandidate + (n / 2).floor
  for i in firstCandidate..lastCandidateFactor do
    next unless sieve[i]
    for j in 2..(lastCandidate / i).floor do
      sieve[i * j] = false
    end
  end
  sieve.reject! { |k,v| not v }
  sieve.keys.sort.each { |n| known.push(n) }
end

def nthPrime(n, known)
  currentCount = known.length
  print "Want prime #", n, "; currently have ", currentCount, "\n"
  while currentCount < n do
    extendBy(known, [n - known.length, 100000].min * 10)
    currentCount = known.length
  end
  return known[n - 1]
end

def testAppend(ar, el)
  ar.push(el)
end

knownPrimes = []
puts extendBy(knownPrimes, 5)
puts knownPrimes.length

for i in 1..6 do
  puts nthPrime(i, knownPrimes)
end

puts nthPrime(10001, knownPrimes)