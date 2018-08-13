import collection._
import io.Source

object Euler42 {
	var triangulars = (1 to 100).map(n => (n * (n + 1) / 2))
	
	def main(args: Array[String]) {
		val str = Source.fromFile("words.txt").toArray.mkString
		val words = str.replaceAll("\"", "").split(",").filter(isTriangular(_))
		println("Answer is " + words.size)
	}
	
	def isTriangular(word: String): Boolean = {
		isTriangular((0 /: word.toString){ (s, c) => s + (c - 'A'.toInt + 1) })
	}
	
	def isTriangular(n: Int): Boolean = triangulars.contains(n)
}