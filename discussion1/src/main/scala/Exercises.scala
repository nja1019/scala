object Exercises{


	sealed trait Bird
	case class Duck() extends Bird
	case class Goose() extends Bird
	//Week 1 discussion
	// def map[A,B](f: A =>B, xs: List[A]): List[B] = xs match {
	// 	case Nil => Nil
	// 	case head :: tail => f(head) :: map[A,B](f, tail)
	// }

	// def exercise1(xs: List[Bird]) : List[String] = {
 //    	val f = (x: Bird) => x match {
	//       case Duck() => "dog food"
	//       case Goose() => "pate"
 //    	}
 //    	map(f, xs)
 //  	}

  	def fold[A,B](acc: B, f: (B,A) => B, xs: List[A]): B = {
  		xs match {
  			case Nil => acc
  			case h :: rest => fold(f(acc, h), f, rest)
  		}
  	}

	def exercise3(xs: List[Bird]): Int = {
		val f = (total: Int, x: Bird) => x match{
			case Duck() => 1 + total
			case Goose() => 10 + total
		}
		fold(0, f, xs)
	}
	
}