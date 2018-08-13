import scala.collection._

object Euler26 {
	def main(args: Array[String]) {
		check(2, 0)
		check(3, 1)
		check(6, 1)
		check(7, 6)
		
		var maxRecur, forValue = -1
		for (i <- 2 to 1000) {
			val recur = recipRecurLen(i)
			if (recur > maxRecur) {
				maxRecur = recur
				forValue = i
			}
		}
		println("Max recurrence length is " + maxRecur + " for 1/" + forValue)
	}
	
	def check(n: Int, exp: Int) {
		val v = recipRecurLen(n)
		if (v != exp) {
			throw new Exception("For " + n + " expected " + exp + "; got " + v)
		}
	}
	
	def recipRecurLen(n: Int): Int = {
		var work = mutable.LinkedHashSet[Int]()
		var curr = 10
		while (true) {
			val rem = curr % n
			if (rem == 0) {
				return 0
			}
			if (work.contains(rem)) {
				val recurrence = work.dropWhile(_ != rem)
				return recurrence.size
			}
			work += rem
			curr = rem * 10
		}
		-1 // Should never get here...
	}
}