object Euler40 {
	def main(args: Array[String]) {
		var b = new StringBuilder()
		var i = 1
		while (b.size < 1000000) {
			b ++= i.toString
			i += 1
		}
		println("Digit 1 is " + b(0))
		println("Digit 10 is " + b(9))
		println("Digit 100 is " + b(99))
		println("Digit 1000 is " + b(999))
		println("Digit 10000 is " + b(9999))
		println("Digit 100000 is " + b(99999))
		println("Digit 1000000 is " + b(999999))
		val ans = b(0).asDigit * b(9).asDigit * b(99).asDigit *
		    b(999).asDigit * b(9999).asDigit * b(99999).asDigit *
		    b(999999).asDigit
		println("Answer is " + ans)
	}
}