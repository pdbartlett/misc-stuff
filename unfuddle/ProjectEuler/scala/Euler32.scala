import collection.mutable._

object Euler32 {
	var answers = Set[Int]()
	
	def main(args: Array[String]) {
		for {
			i1 <- 0 to 8
			i2 <- 0 to 7
			i3 <- 0 to 6
			i4 <- 0 to 5
			i5 <- 0 to 4
			i6 <- 0 to 3
			i7 <- 0 to 2
			i8 <- 0 to 1
			i9 <- 0 to 0
		} {
			val avail = ListBuffer[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
			val v1 = avail.remove(i1)
			val v2 = avail.remove(i2)
			val v3 = avail.remove(i3)
			val v4 = avail.remove(i4)
			val v5 = avail.remove(i5)
			val v6 = avail.remove(i6)
			val v7 = avail.remove(i7)
			val v8 = avail.remove(i8)
			val v9 = avail.remove(i9)
			tryPoss(v1, v2, v3, v4, v5, v6, v7, v8, v9)
		}
		
		println("Total is: " + (0 /: answers)(_ + _))
	}
	
	def tryPoss(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int,
		          i6: Int, i7: Int, i8: Int, i9: Int) {
			check(num(i1), num(i2), num(i3, i4, i5, i6, i7, i8, i9))
			check(num(i1), num(i2, i3), num(i4, i5, i6, i7, i8, i9))
			check(num(i1), num(i2, i3, i4), num(i5, i6, i7, i8, i9))
			check(num(i1), num(i2, i3, i4, i5), num(i6, i7, i8, i9))
			check(num(i1, i2), num(i3), num(i4, i5, i6, i7, i8, i9))
			check(num(i1, i2), num(i3, i4), num(i5, i6, i7, i8, i9))
			check(num(i1, i2), num(i3, i4, i5), num(i6, i7, i8, i9))
			check(num(i1, i2), num(i3, i4, i5, i6), num(i7, i8, i9))
			check(num(i1, i2, i3), num(i4), num(i5, i6, i7, i8, i9))
			check(num(i1, i2, i3), num(i4, i5), num(i6, i7, i8, i9))
			check(num(i1, i2, i3), num(i4, i5, i6), num(i7, i8, i9))
			check(num(i1, i2, i3), num(i4, i5, i6, i7), num(i8, i9))
			check(num(i1, i2, i3, i4), num(i5), num(i6, i7, i8, i9))
			check(num(i1, i2, i3, i4), num(i5, i6), num(i7, i8, i9))
			check(num(i1, i2, i3, i4), num(i5, i6, i7), num(i8, i9))
			check(num(i1, i2, i3, i4), num(i5, i6, i7, i8), num(i9))
	}
	
	def check(x: Int, y: Int, z: Int) {
		if (z == x * y) {
			println("Got " + x + " * " + y + " = " + z)
			answers += z
		}
	}
	
	def num(xs: Int*) = (0 /: xs)(10 * _ + _)
}