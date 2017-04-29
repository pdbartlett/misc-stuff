object Euler28 {
	def main(args: Array[String]) {
		var total = 1
		var curr = 1
		var step = 2
		for (level <- 1 to 500) {
			for (corner <- 1 to 4) {
				curr += step
				total += curr
			}
			step += 2
		}
		println("Total = " + total)
	}
}