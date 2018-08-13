import collection.mutable.MutableList
import util.control.Breaks._

object Euler50 {
  def main(args: Array[String]) {
    val ans = solveFor(Integer.valueOf(args(0)))
    println("Answer is: " + ans)
  }
  
  def solveFor(target: Int): Int = {
    val sieve = new Array[Boolean](target)
    sieve(0) = true
    sieve(1) = true
    var curr = 2
    while (curr < target) {
      if (!sieve(curr)) {
        var mark = 2 * curr
        while (mark < target) {
          sieve(mark) = true
          mark += curr
        }
      }
      curr += 1
    }
    val primes = MutableList[Int]()
    for (i <- 0 until target) {
      if (!sieve(i)) primes += i
    }
    val n = primes.size
    var best = 1
    var ans = -1
    for (j <- 0 until n) {
      if (j + best >= n) return ans
      var sum = 0
      var k = j
      breakable { while (k < n) {
        sum += primes(k)
        val terms = (k - j) + 1
        if (sum >= target) {
          if (terms <= best) return ans
          break
        }
        if (!sieve(sum)) {
          if (best < terms) {
            best = terms
            ans = sum
            println("Current best: " + ans + " with " + best + " terms ("
              + primes(j) + " to " + primes (k) + ")")
          }
        }
        k += 1
      } }
    }
    ans
  }
}
