object Euler33 {
	def main(args: Array[String]) {
		var ns, ds = List[Int]()
		for {
			d <- 10 to 99
			n <- 10 until d
			td = d / 10
			ud = d % 10
			tn = n / 10
			un = n % 10
			if (td == un && (ud * n == tn * d)) ||
			   (tn == ud && (un * d == td * n))
		} {
			ns = n :: ns
			ds = d :: ds
		}
		var an = (1 /: ns)(_ * _)
		var ad = (1 /: ds)(_ * _)
		for (f <- 2 to math.sqrt(ad).toInt) {
			while (an % f == 0 && ad % f == 0) {
				an /= f
				ad /= f 
			}
		}
		println("Answer is " + an + "/" + ad)
	}
}