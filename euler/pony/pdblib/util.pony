class Util
  fun prints[A: Stringable val](to: OutStream, a: A) =>
    to.print(a.string())

  fun writes[A: Stringable val](to: OutStream, a: A) =>
    to.write(a.string())

  fun printarray[A: Stringable val](to: OutStream, arr: Array[A]) =>
    printit[A](to, arr.values())

  fun printit[A: Stringable val](to: OutStream, it: Iterator[A]) =>
    for elem in it do
      writes[A](to, elem)
      to.write(", ")
    end
    to.print("")

  fun printgrid[A: Stringable val](to: OutStream, grid: Array[Array[A]]) =>
    for row in grid.values() do
      printarray[A](to, row)
    end
