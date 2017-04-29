object Euler39 {
	def main(args: Array[String]) {
		var maxC, maxP = 0
		var rats = Map[Int, List[(Int, Int, Int)]]()
		for {
			p <- 3 to 1000
			a <- 1 to p - 2
			b <- 1 to (p - (a + 1))
			c = p - (a + b)
			if (a * a == (b * b + c * c))
			rat = (a, b, c)
		} {
			if (rats.contains(p))
				rats += (p -> (rat :: rats(p)))
		  else
		    rats += (p -> List(rat))
			val c = rats(p).size
			if (c > maxC) {
				maxC = c
				maxP = p
			}
		}
		println("Maximum is for " + maxP + ": " + rats(maxP).mkString(", "))
	}
}