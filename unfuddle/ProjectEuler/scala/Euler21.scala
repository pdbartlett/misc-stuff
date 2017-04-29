object Euler21 {
	def main(args: Array[String]) {
		val amicable = amicableLessThan(10000)
		println(amicable.mkString(","))
		
		val ans = (0 /: amicable) ( _ + _ )
		println("Answer is " + ans)
	}
	
	def amicableLessThan(n: Int) = {
		for (i <- 2 until n if isAmicable(i)) yield i
	}
	
	def isAmicable(n: Int) = {
		val candidate = sumDivisors(n)
		val res = candidate != n && sumDivisors(candidate) == n
		if (res) { println("Amicable: " + n + " and " + candidate) }
		res
	}
	
	def sumDivisors(n: Int) = (0 /: divisors(n)) ( _ + _ )
	
	def divisors(n: Int) = {
		var divs = Set[Int](1)
		for (i <- 2 to math.sqrt(n).toInt if n % i == 0) {
			divs += i
			divs += n / i
		}
		divs
	}
}