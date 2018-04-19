
class TrivialTestSuite extends org . scalatest . FunSuite {
	import Regexes._
	test(" The Regexes object must be defined ") {
		val regexes : hw . regex . RegexLike = Regexes
	}
	test("test evenParity"){
		assert(evenParity.pattern.matcher("203").matches() == false)
		assert(evenParity.pattern.matcher("00").matches() == true)
		assert(evenParity.pattern.matcher("0").matches() == true)
		assert(evenParity.pattern.matcher("002").matches() == true)
		assert(evenParity.pattern.matcher("30").matches() == false)
		assert(evenParity.pattern.matcher("33").matches() == true)
	}
}