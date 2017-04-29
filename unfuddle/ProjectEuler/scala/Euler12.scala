import scala.collection._

object Euler12 {

  def main(args: Array[String]) {
	  var n = 1
	  while (true) {
		  n += 1
		  val t = getNthTriangularNumber(n)
		  if (getFactors(t).size > 500) {
			  println("Answer: " + t)
			  return
		  }
	  }
  }

  def getNthTriangularNumber(n: Int) = n * (n + 1) / 2

  def getFactors(n: Int): Set[Int] = {
	  var factors = Set[Int]()
	  for (i <- 1 to math.sqrt(n).toInt) {
		  if (n % i == 0) {
			  factors += i
			  factors += (n / i)
		  }
	  }
	  factors
  }
}