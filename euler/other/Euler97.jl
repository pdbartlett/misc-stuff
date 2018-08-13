n = BigInt(28433)
for i = 1:7830457
  global n
  n *= 2
  n %= BigInt(1e10)
end
n += 1
println(n)
