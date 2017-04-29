import collection._

object Euler37 {
	def main(args: Array[String]) {
		var count, total = 0
		var curr = 10
		while (count < 11) {
			if (isPrime(curr) &&
			    truncl(curr).forall(isPrime(_)) &&
			    truncr(curr).forall(isPrime(_))) {
				println("Got " + curr)
				count += 1
				total += curr
			}
			curr += 1
		}
		
		println("Answer is " + total)
	}

  def truncl(n: Int) = {
	  val s = n.toString
	  for (i <- 1 until s.length) yield s.drop(i).toInt
  }

  def truncr(n: Int) = {
	  val s = n.toString
	  for (i <- 1 until s.length) yield s.dropRight(i).toInt
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
		if (n < 2) return false
		for (i <- 2 to math.sqrt(n).toInt) {
  		if (n % i == 0) {
	      return false
	    }
		}
		true
	}
}