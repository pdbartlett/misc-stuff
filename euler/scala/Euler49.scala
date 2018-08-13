import collection._

object Euler49 {
	def main(args: Array[String]) {
		for {
			a <- 1000 to 9997
			s <- 1 to ((9999 - a) / 2)
			b = a + s
			c = b + s
			if arePermutations(a, b, c)
			if arePrimes(a, b, c)
		} {
			println("Got " + a + ":" + b + ":" + c)
		}
	}
	
	def arePermutations(a: Int, b: Int, c: Int) = {
		val sa = a.toString.sorted
		(sa == b.toString.sorted) && (sa == c.toString.sorted)
	}
	
	def arePrimes(a: Int, b: Int, c: Int) = isPrime(a) && isPrime(b) && isPrime(c)
	
	var primeCache = mutable.Map[Int, Boolean]()
	
	def isPrime(n: Int): Boolean = {
		primeCache.get(n) match {
			case Some(whether) => whether
			case None => {
				val whether = doIsPrime(n)
				primeCache += (n -> whether)
				whether
			}
	  }
	}
	
	def doIsPrime(n: Int): Boolean = {
		for (i <- 2 to math.sqrt(n).toInt) {
  		if (n % i == 0) {
	      return false
	    }
		}
		true
	}
}