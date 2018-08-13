def fibonacciTo(max)
  thisNum, nextNum = 1, 1
  while thisNum < max
    yield thisNum
    thisNum, nextNum = nextNum, nextNum + thisNum
  end
end

total = 0
fibonacciTo(4000000) { |n| total += n if n.remainder(2) == 0 }
print total