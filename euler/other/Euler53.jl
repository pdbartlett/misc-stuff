c=0
for n=1:100, k=1:n
  global c
  est = try
    binomial(n,k)
  catch
    2e6
  end
  if est > 1e6
    c+=1
  end
end
println(c)