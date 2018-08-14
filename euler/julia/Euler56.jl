pint(s) = parse(Int, s)

max = 0
for a = 1:100, b = 1:100
  global max
  n = BigInt(a) ^ BigInt(b)
  digits = split(string(n), "")
  s = sum(pint, digits)
  if s > max
    max = s
  end
end
println(max)