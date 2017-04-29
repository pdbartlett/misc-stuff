import collection._

object Euler46 {
	def main(args: Array[String]) {
		println("Answer is " + findIt)
	}
	
	def findIt: Long = {
		var n = 3L
		while (true) {
			if (!isPrime(n)) {
				if (isAnswer(n)) {
					return n
				}
			}
			n += 2L
		}
		-1L  // Cannot get here, but needed by compiler.
	}
	
	def isAnswer(n: Long): Boolean = {
		print("Trying " + n + "... ")
		for (i <- 1L to math.sqrt(n / 2).toLong) {
			val rem = n - 2 * i * i
			if (isPrime(rem)) {
				println("Got " + rem + " + 2 * " + i + "^2")
				return false
			}
		}
		true
	}
	
	var primeCache = mutable.Map[Long, Boolean]()
	
	def isPrime(n: Long): Boolean = {
		primeCache.get(n) match {
			case Some(whether) => whether
			case None => {
				val whether = doIsPrime(n)
				primeCache += (n -> whether)
				whether
			}
	  }
	}
	
	def doIsPrime(n: Long): Boolean = {
		for (i <- 2L to math.sqrt(n).toLong) {
  		if (n % i == 0) {
	      return false
	    }
		}
		true
	}
}