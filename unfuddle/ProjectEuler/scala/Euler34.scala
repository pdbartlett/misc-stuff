object Euler34 {
	def main(args: Array[String]) {
		var fact = Map[Int, Int]()
		for (i <- 0 to 9) {
			val f = (1 /: (2 to i))(_ * _)
			fact += (i -> f)
		}
		
		var total = 0
		for (n <- 10 to 10000000) {
			val sumf = (0 /: n.toString){ (s, c) => s + fact(c.asDigit) }
			if (n == sumf) {
				println("Got " + n)
				total += n
			}
		}
		
		println("Answer is " + total)
	}
}