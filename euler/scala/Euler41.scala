import collection._

object Euler41 {
  def main(args: Array[String]) {
    var max = 0
    for {
	    len <- 2 to 9
	    d1 <- mkRange(len, 1)
	    d2 <- mkRange(len, 2)
	    d3 <- mkRange(len, 3)
	    d4 <- mkRange(len, 4)
	    d5 <- mkRange(len, 5)
	    d6 <- mkRange(len, 6)
	    d7 <- mkRange(len, 7)
	    d8 <- mkRange(len, 8)
	    d9 <- mkRange(len, 9)
    } {
	    var avail = mutable.ListBuffer[Int]()
	    for (i <- 1 to len) avail += i
	    var n = avail.remove(d1 - 1)
	    n *= 10; n += avail.remove(d2 - 1)
	    if (d3 != 0) {
		    n *= 10; n += avail.remove(d3 - 1)
	    }
	    if (d4 != 0) {
		    n *= 10; n += avail.remove(d4 - 1)
	    }
	    if (d5 != 0) {
		    n *= 10; n += avail.remove(d5 - 1)
	    }
	    if (d6 != 0) {
		    n *= 10; n += avail.remove(d6 - 1)
	    }
	    if (d7 != 0) {
		    n *= 10; n += avail.remove(d7 - 1)
	    }
	    if (d8 != 0) {
		    n *= 10; n += avail.remove(d8 - 1)
	    }
	    if (d9 != 0) {
		    n *= 10; n += avail.remove(d9 - 1)
	    }
	    if (isPrime(n)) {
		    println("Got " + n)
		    if (n > max) {
			    max = n
		    }
	    }
    }
    println("Answer is " + max)
  }

  def mkRange(len: Int, pos: Int) = {
	  if (pos > len) (0 to 0) else (1 to len - pos + 1)
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