object Euler23 {
	def main(args: Array[String]) {
		val abundant = (2 to 28123).filter(isAbundant).toSet
		val reqd = (1 to 28123).filter(n => !isSumOfTwo(n, abundant))
		val ans = (0 /: reqd) ( _ + _)
		println("Answer is " + ans)
	}
	
	def isSumOfTwo(n: Int, pool: Set[Int]): Boolean = {
		for (a <- pool if a < n) {
			if (pool.contains(n - a)) { return true }
		}
		false
	}
	
	def isAbundant(n: Int) = sumDivisors(n) > n
	
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