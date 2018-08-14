len(n) = length(string(n))

c = 0
n = 1
while true
  global c, n
  if len(BigInt(9^n)) < n
    break
  end
  for i = 0:9
    maybe = BigInt(i^n)
    if len(maybe) == n
      println(i, "^", n, "=", maybe)
      c += 1
    end
  end
  n += 1
end
println(c)