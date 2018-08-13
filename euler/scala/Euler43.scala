import collection._

object Euler43 {
  def main(args: Array[String]) {
	  var sum = 0L
	  for {
		  i1 <- 0 to 9
		  i2 <- 0 to 8
		  i3 <- 0 to 7
		  i4 <- 0 to 6
		  i5 <- 0 to 5
		  i6 <- 0 to 4
		  i7 <- 0 to 3
		  i8 <- 0 to 2
		  i9 <- 0 to 1
	  } {
		  var avail = mutable.ListBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
		  val d1 = avail.remove(i1)
		  val d2 = avail.remove(i2)
		  val d3 = avail.remove(i3)
		  val d4 = avail.remove(i4)
		  val d5 = avail.remove(i5)
		  val d6 = avail.remove(i6)
		  val d7 = avail.remove(i7)
		  val d8 = avail.remove(i8)
		  val d9 = avail.remove(i9)
		  val d10 = avail.remove(0)
		  if (check(d2, d3, d4, 2) &&
		      check(d3, d4, d5, 3) &&
		      check(d4, d5, d6, 5) &&
		      check(d5, d6, d7, 7) &&
		      check(d6, d7, d8, 11) &&
		      check(d7, d8, d9, 13) &&
		      check(d8, d9, d10, 17)) {
			  val digits = List(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
			  val num = (0L /: digits){ (s, v) => 10 * s + v }
			  println("Got " + num)
			  sum += num
  		}
	  }
	  println("Answer = " + sum)
  }

  def check(p1: Int, p2: Int, p3: Int, f: Int) = {
	  (100 * p1 + 10 * p2 + p3) % f == 0
	}
}