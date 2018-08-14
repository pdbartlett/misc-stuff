c = 0
for n = 3:1000
  global c
  v = Rational{BigInt}(1, 2)
  for i = 1:n-1
    v += 2
    v = 1/v
  end
  v += 1
  if length(string(numerator(v))) > length(string(denominator(v)))
    c += 1
  end
end
println(c)