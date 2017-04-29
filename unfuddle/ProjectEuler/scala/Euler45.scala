object Euler45 {
	// Tn = n * (n + 1) / 2
	// Pn = n * (3n - 1) / 2
	// Hn = n * (2n - 1)
	def main(args: Array[String]) {
		if (!isPentAndHex(40755)) {
		  throw new Exception("Uh-oh!")	
		}
		
		var found = 0L
		var n = 286L
		while (found == 0) {
			val t = n * (n + 1) / 2
			//println("Trying n=" + n + ", Tn=" + t)
			if (isPentAndHex(t)) {
				println("Got " + t)
				found = t
			}
			n += 1
		}
		println("Answer is " + found)
	}
	
	def isPentAndHex(n: Long): Boolean = {
		val proot = isqrt(1 + 24 * n)
		if ((proot == -1) || (proot % 6 != 5)) return false
		
		val hroot = isqrt(1 + 8 * n)
		(hroot != -1) && (hroot % 4 == 3)
	}
	
	def isqrt(n: Long) = {
		val root = math.sqrt(n).toInt
		if (n == root * root) root else -1
	}
}