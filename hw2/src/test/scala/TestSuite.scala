import Homework2._

class TestSuite extends org.scalatest.FunSuite {


	 test("map2 with add") {
        def add(x: Int, y: Int): Int = x + y
        assert(map2(add, List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
        assert(map2(add, List(), List()) == List())
	}

	test("zip test 1") {
	    assert(zip(List(1, 2, 3), List(4, 5, 6)) == List((1,4), (2, 5), (3, 6)))
	    assert(zip(List(), List()) == List())
	    assert(zip(List(0,0,0), List(1,2,3)) == List((0,1), (0,2), (0,3)))
	}

	 test("zip test 2") {
        assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) ==
			List(("George", "Washington"), ("Teddy", "Roosevelt")))
    }

    test("flatten test") {
		assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
		assert(flatten(List(List())) == List())
		assert(flatten(List(List(1, 2), List(3, 4), List(5,6))) == List(1, 2, 3, 4, 5,6))

	}

	test("append method works"){
		assert(append(List(1,2), List(3,4)) == List(1,2,3,4))
		assert(append(List("One", "Two"), List("Three", "Four")) == List("One", "Two", "Three", "Four"))
		assert(append(List(1), List(3,4)) == List(1,3,4))
		assert(append(List(), List()) == List())
	}

	test("flatten3 test"){
		assert(flatten3(List(List(List(1,2),List(3,4)))) == List(1,2,3,4))
		assert(flatten3(List(List(List(1,2),List(3,4), List(1,2)))) == List(1,2,3,4,1,2))
		assert(flatten3(List(List(List()))) == List())
	}

	test("partition test"){
		def isEven(x: Int): Boolean = {
			x % 2 == 0
		}
		assert(partition(isEven, List(1,2,3,4)) == (List(2,4), List(1,3)))
  		assert(partition(isEven, List(1,2,3,4,5,6)) == (List(2,4,6), List(1,3,5)))
  		assert(partition(isEven, List(2,4,6)) == (List(2,4,6), Nil))
 		assert(partition(isEven, List(1,3,5)) == (Nil, List(1,3,5)))
 		assert(partition(isEven, List()) == (List(), List()))
 	}

 	test("mapList test") {
		def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
		assert(mapList(List(1, 2, 3), f) == List(1, 2, 2, 3, 3, 3))
	}

	test("buildList test") {
		def f(x: Int) = x
		def isEven(x: Int): String ={
			if(x % 2 == 0) "True"
			else "False"
		}
	 		
		assert(buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
		assert(buildList(10, isEven) == List("True", "False", "True", "False", "True", "False", "True", "False", "True", "False"))
	}

}