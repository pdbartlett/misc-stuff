import scala.collection._

object Euler27 {
	def main(args: Array[String]) {
		val bounds = 999
		var max = -1
		var va, vb = -1000
		for {
			a <- -bounds to bounds
			b <- -bounds to bounds
		} {
			var n = getPrimesCount(a, b, false)
			if (n > max) {
				max = n
				va = a
				vb = b
			}
		}
		getPrimesCount(va, vb, true)
		println("Answer is " + max + " for a=" + va + " and b=" + vb)
	}
	
	def getPrimesCount(a: Int, b: Int, verbose: Boolean): Int = {
		var n = 0
		while (true) {
			val value = (n * (n + a)) + b
			if (value < 2 || !isPrime(value)) {
				return n
			}
			if (verbose) {
				println("n = " + n + "; prime = " + value)
			}
			n += 1
		}
		-1000 // Should not get here
	}
	
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