object Euler17 {
	def main(args: Array[String]) {
		val test = sumCharsUpTo(5)
		if (test != 19) {
			throw new Exception("Nope. Got " + test + " but expected 19")
		}
		//for (i <- (1 to 1000)) { println(inWords(i)) }
		println("Answer is " + sumCharsUpTo(1000))
	}
	
	def sumCharsUpTo(n: Int) =
	  (0 /: (1 to n)) { (s, i) => s + inWords(i).length }
	
	def inWords(n: Int): String = n match {
		case 1 => "one"
		case 2 => "two"
		case 3 => "three"
		case 4 => "four"
		case 5 => "five"
		case 6 => "six"
		case 7 => "seven"
		case 8 => "eight"
		case 9 => "nine"
		case 10 => "ten"
		case 11 => "eleven"
		case 12 => "twelve"
		case 13 => "thirteen"
		case 14 => "fourteen"
		case 15 => "fifteen"
		case 16 => "sixteen"
		case 17 => "seventeen"
		case 18 => "eighteen"
		case 19 => "nineteen"
		case 20 => "twenty"
		case n: Int if (n < 30) => inWords(20) + inWords(n - 20)
		case 30 => "thirty"
		case n: Int if (n < 40) => inWords(30) + inWords(n - 30)
		case 40 => "forty"
		case n: Int if (n < 50) => inWords(40) + inWords(n - 40)
		case 50 => "fifty"
		case n: Int if (n < 60) => inWords(50) + inWords(n - 50)
		case 60 => "sixty"
		case n: Int if (n < 70) => inWords(60) + inWords(n - 60)
		case 70 => "seventy"
		case n: Int if (n < 80) => inWords(70) + inWords(n - 70)
		case 80 => "eighty"
		case n: Int if (n < 90) => inWords(80) + inWords(n - 80)
		case 90 => "ninety"
		case n: Int if (n < 100) => inWords(90) + inWords(n - 90)
		case 1000 => "onethousand"
		case n: Int if (n % 100 == 0) => inWords(n / 100) + "hundred"
		case n: Int => {
			val rounded = 100 * (n / 100)
			inWords(rounded) + "and" + inWords(n - rounded)
	  }
	}
}