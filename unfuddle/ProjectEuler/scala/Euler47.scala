import collection._

object Euler47 {
	def main(args: Array[String]) {
		var n = 2
		var count = 0
		while (count < 4) {
			val pfs = getPrimeFactors(n)
			val c = pfs.toSet.size
			if (c == 4) {
				count += 1
			} else {
				count = 0
			}
			n += 1
		}
		
		println("Answer is " + (n - 4))
	}
	
	def getPrimeFactors(n: Long) = {
		var pfs = List[Long]()
		var value = n
		var pf = 2
		while (value > 1) {
			if (value % pf == 0L) {
				pfs = pf :: pfs
				value /= pf
			} else {
				pf += 1
			}
		}
		pfs
	}
}