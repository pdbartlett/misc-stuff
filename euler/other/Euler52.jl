function encode(n)
  sort(split(string(n), ""))
end

n = BigInt(1)
while true
  global n
  if encode(n) == encode(2n) == encode(3n) == encode(4n) == encode(5n) == encode(6n)
    println(n)
    break
  end
  n += 1
end 
