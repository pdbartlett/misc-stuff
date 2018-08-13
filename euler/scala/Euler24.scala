import scala.collection._

object Euler24 {
	def main(args: Array[String]) {
		var counter = 0
		for {
			p10 <- 0 to 9
			p9  <- 0 to 8
			p8  <- 0 to 7
			p7  <- 0 to 6
			p6  <- 0 to 5
			p5  <- 0 to 4
			p4  <- 0 to 3
			p3  <- 0 to 2
			p2  <- 0 to 1
			p1  <- 0 to 0
		} {
			counter += 1
      if (counter == 1000000) {
				val avail = mutable.ListBuffer[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
				var taken = mutable.ListBuffer[Int]()
				taken += avail(p10)
				avail.remove(p10)
				taken += avail(p9)
				avail.remove(p9)
				taken += avail(p8)
				avail.remove(p8)
				taken += avail(p7)
				avail.remove(p7)
				taken += avail(p6)
				avail.remove(p6)
				taken += avail(p5)
				avail.remove(p5)
				taken += avail(p4)
				avail.remove(p4)
				taken += avail(p3)
				avail.remove(p3)
				taken += avail(p2)
				avail.remove(p2)
				taken += avail(p1)
				avail.remove(p1)
	      println(taken.mkString)
	      return
			}
		}
  }
}