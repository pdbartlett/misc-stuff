import collection._

object Euler35 {
	def main(args: Array[String]) {
		println("Test " + rot(123).mkString(", "))
		var count = 0
  	for (i <- 2 until 1000000) {
			if (rot(i).forall(isPrime(_))) count += 1
		}
		println("Answer is " + count)
	}
	
	def rot(n: Int) = {
		val s = n.toString
		for {
			i <- 0 until s.length
			(b, e) = s.splitAt(i)
		}	yield (e + b).toInt
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