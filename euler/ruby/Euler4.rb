# Largest palindrome that is product of two 3-digit numbers.

def palindrome?(n)
  str = n.to_s
  return str == str.reverse
end

candidates = []
100.upto(999) do |n|
  n.upto(999) do |m|
    product = n * m
    candidates.push(product) if palindrome?(product)
  end
end

puts candidates.max