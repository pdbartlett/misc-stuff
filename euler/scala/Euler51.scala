import Primes._

object Euler51 extends App {

  (1 until 100).filter(isPrime(_)).foreach(println(_))
}
