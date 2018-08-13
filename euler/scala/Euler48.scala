import math.BigInt.int2bigInt

object Euler48 {
	def main(args: Array[String]) {
		val total = (BigInt(0) /: (1 to 1000)) { (s, i) => s + i.pow(i) }
		println("Answer is " + total.toString.takeRight(10))
	}
}