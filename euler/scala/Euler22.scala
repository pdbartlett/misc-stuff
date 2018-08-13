import scala.io.Source

object Euler22 {
	def main(args: Array[String]) {
		if (f("COLIN") != 53) {
			throw new Exception("Nope, got " + f("COLIN") + " instead of 53.")
		}
		val str = Source.fromFile("names.txt").toArray.mkString
		val names = str.replaceAll("\"", "").split(",")
		util.Sorting.quickSort(names)
    val sum = (0 /: names.zipWithIndex) { (a, p) => a + (p._2 + 1) * f(p._1) }
    println("Answer is " + sum)
	}
	
	def f(s: String) = (0 /: s) { (a, c) => a + c.toInt - 'A'.toInt + 1 }
}