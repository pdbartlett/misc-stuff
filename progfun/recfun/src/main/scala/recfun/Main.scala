package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balance(0, chars)
  
  def balance(open: Int, chars: List[Char]): Boolean = {
    val newOpen = chars.head match {
      case '(' => open + 1
      case ')' => open - 1
      case _   => open
    }
    if (newOpen < 0) {
      false
    } else {
      val tail = chars.tail
      if (tail.isEmpty) (newOpen == 0) else balance(newOpen, tail)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =  {
    if (coins.isEmpty || money < 0) {
      0 // either we're completely done, or we've made a bad choice of coin
    } else if (money == 0) {
      1 // we've found a new way, so count it
    } else {
      // total ways is sum of ways to make up the rest of the amount, plus ways
      // to make the total amount without using this coin
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
