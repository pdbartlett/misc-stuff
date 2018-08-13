object Euler25 {
	def main(args: Array[String]) {
		var nm2 = BigInt("1")
		var nm1 = BigInt("1")
		var n = nm1 + nm2
		var i = 3
		while (n.toString.length < 1000) {
			nm2 = nm1
			nm1 = n
			n = nm1 + nm2
			i += 1
		}
		println("Answer is " + i)
	}
}