object Euler19 {
  def main(args: Array[String]) {
	  var count = 0
	  var dow = 1
	  for {
		  year <- 1900 to 2000
		  month <- 1 to 12
		} {
			if (year > 1900 && dow == 0) { count += 1 }
			month match {
				case  1 => dow += 4
				case  2 if (year % 400 == 0) => dow += 6
				case  2 if (year % 100 == 0) => dow += 0
				case  2 if (year %   4 == 0) => dow += 6
				case  2 => dow += 0
				case  3 => dow += 4
				case  4 => dow += 5
				case  5 => dow += 4
				case  6 => dow += 5
				case  7 => dow += 4
				case  8 => dow += 4
				case  9 => dow += 5
			  case 10 => dow += 4
			  case 11 => dow += 5
			  case 12 => dow += 4
			}
			dow %= 7
		}
	  println("Answer is " + count)
  }
}