function ispalindrome(n)
  s = string(n)
  s == reverse(s)
end

function radd(n)
  n + parse(BigInt, reverse(string(n)))
end

function islychrel(n)
  for i = 1:50
    n = radd(n)
    if ispalindrome(n)
      return false
    end
  end
  true
end

c=0
for i=1:10000
  global c
  if islychrel(BigInt(i))
    c += 1
  end
end

println(c)