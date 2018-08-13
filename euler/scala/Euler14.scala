object Euler14 {
	
	def main(args: Array[String]) {
		if (chainLength(13) != 10) {
			throw new Exception("Nope!")
		}
		var longestChainLength, numWithLongestChain = -1
		for (i <- 1 until 1000000) {
			val length = chainLength(i)
			if (length > longestChainLength) {
				longestChainLength = length
				numWithLongestChain = i
			}
//			println("Done " + i)
		}
		println("Answer is: " + numWithLongestChain)
  }

  def chainLength(n: Int): Int = {
//	  println("******************************")
//	  println("For n=" + n)
	  var len = 1	
	  var curr: Long = n
	  while (curr != 1) {
		  curr = if (curr % 2 == 0) curr / 2 else 3 * curr + 1
		  len += 1
//		  println("Term " + len + " = " + curr)
	  }
	  len
  }
}