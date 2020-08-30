my_globals = globals().copy()
my_locals = locals().copy()
while True:
  try:
    x = input('--> ')
  except:
    break
  try:
    print(eval(x, my_globals, my_locals))
  except:
    exec(x, my_globals, my_locals)
