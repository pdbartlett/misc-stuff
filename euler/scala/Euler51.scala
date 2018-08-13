package solutions

import common.Primes._

object P51 extends App {

  (1 until 100).filter(isPrime(_)).foreach(println(_))
}