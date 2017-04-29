object Euler16 {
	def main(args: Array[String]) {
		val num = BigInt("2").pow(1000)
		val sum = (0 /: num.toString) { (s, c) => s + c.asDigit }
		println("Answer is " + sum)
	}
}