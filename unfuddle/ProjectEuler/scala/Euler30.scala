import math.BigInt._

object Euler30 {
	var pow5 = Map[Int, Int]()
	
	def main(args: Array[String]) {
		for (i <- 0 to 9) {
			pow5 += (i -> i.pow(5).toInt)
		}
		
		val nums = (10 to 1000000).filter(isReqd(_))
		println("Nums are " + nums.mkString(", "))
		println("Sum is " + (0 /: nums)(_ + _))
	}
	
	def isReqd(n: Int): Boolean = {
		n == (0 /: n.toString) { (a, c) => a + pow5(c.asDigit) }
	}
}