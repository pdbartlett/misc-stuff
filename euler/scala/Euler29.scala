import collection._
import math.BigInt._

object Euler29 {
	def main(args: Array[String]) {
		var terms = mutable.Set[BigInt]()
		for {
			a <- 2 to 100
			b <- 2 to 100
		} {
			terms += a.pow(b)
		}
		println("Num terms = " + terms.size)
	}
}