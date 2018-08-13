object Primes {

  private var cache: Map[Int, Boolean] = Map(0 -> false, 1 -> false)
  
  def isPrime(num: Int): Boolean = cache.getOrElse(num, {
    val p = !(2.until(Math.sqrt(num).toInt + 1).exists(n => isPrime(n) && (num % n == 0)))
    cache += (num -> p)
    p
  })
}
