object Euler36 {
	def main(args: Array[String]) {
		var total = 0
		for (i <- 1 until 1000000) {
			if (isPalin(i.toString) && isPalin(i.toBinaryString)) {
				println("Got " + i)
				total += i
			}
		}
		println("Answer is " + total)
	}
	
	def isPalin(s: String) = (s == s.reverse)
}