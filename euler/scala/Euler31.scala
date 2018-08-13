import collection._

object Euler31 {
  val coins = immutable.List[Int](200, 100, 50, 20, 10, 5, 2, 1)
  var combs = mutable.ListBuffer[immutable.List[Int]]()

  def main(args: Array[String]) {
	  findCombs(200, 0, immutable.List[Int]())
	  println("Answer is " + combs.size)
  }

  def findCombs(value: Int, coinIndex: Int, comb: immutable.List[Int]) {
		val thisCoin = coins(coinIndex)
	  if (coinIndex == coins.size - 1) {
		  val thisComb = (value / thisCoin) :: comb
		  println("Combination: " + thisComb.mkString(", "))
		  combs += thisComb
		  return
		}
	  for (i <- 0 to value / thisCoin) {
		  findCombs(value - i * thisCoin, coinIndex + 1, i :: comb)
	  }
  }
}