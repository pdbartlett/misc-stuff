import collection._

object Euler38 {
	def main(args: Array[String]) {
		var max = -1
		for (i <- 1 to 10000) {
			val avail = mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
			var curr = i
			var pan = ""
			var ok = true
			while (ok && !avail.isEmpty) {
				for (d <- curr.toString.map(_.asDigit)) {
					if (avail.contains(d))
					  avail.remove(d)
					else
					  ok = false
				}
				pan += curr.toString
				curr += i
			}
			if (ok) {
				println("Got " + pan + " from " + i)
				if (pan.toInt > max) {
					max = pan.toInt
				}
			}
		}
		println("Max was " + max)
	}
}