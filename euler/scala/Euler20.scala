object Euler20 {
	def main(args: Array[String]) {
		val num = factorial(100)
		println("100! = " + num.toString)
		val sum = (0 /: num.toString) { (s, c) => s + c.asDigit }
		println("Answer is " + sum)
	}
	
	def factorial(n: Int): BigInt = {
		var fact = BigInt(1)
		for (i <- 2 to n) { fact *= BigInt(i)}
		fact
	}
}