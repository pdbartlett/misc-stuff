object Euler44 {
	def main(args: Array[String]) {
		val pc = 1000000
		var pentMap = Map[Long, Long]()
		for (n <- 1L to pc) {
			val p = n * (3 * n - 1) / 2
			pentMap += (n -> p)
		}
		val pentSet = pentMap.values.toSet
    val c = 5000
		var maxs = 0L
		var mind = 0L
		for {
			j <- 1 to c
			k <- 1 until j
			pj = pentMap(j)
			pk = pentMap(k)
			s = pj + pk
			d = pj - pk
			if (pentSet.contains(d))
		} {
			if (s > maxs) {
				maxs = s
			} else {
  			if (pentSet.contains(s)) {
	  	  	if (mind == 0 || d < mind) {
		  	  	mind = d
			    }
			  }
			}
		}
		println("Best answer was " + mind)
		if (maxs > pentMap(pc)) {
			throw new Exception("Max sum was not precalculated: " +
			    maxs + " > " + pentMap(pc))
		}
	}
}