import collection._

object Euler45a {
	def main(args: Array[String]) {
		println("Answer = " + findIt)
	}
	
	def findIt: Long = {
		val ts, ps, hs = mutable.Set[Long]()
		var i = 285L
		var j = 165L
		var k = 143L
		while (true) {
			i += 1
			val t = i * (i + 1) / 2
			if (ps.contains(t) && hs.contains(t)) return t else ts += t

			j += 1
			val p = j * (3 * j - 1) / 2
			if (ts.contains(p) && hs.contains(p)) return p else ps += p

			k += 1
			val h = k * (2 * k - 1)
			if (ts.contains(h) && ps.contains(h)) return h else hs += h
		}
		-1 // Cannot get here, but required by compiler.
	}
}