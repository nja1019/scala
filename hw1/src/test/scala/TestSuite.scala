import Lecture1._

class TestSuite extends org.scalatest.FunSuite {
	test("oddNumbers properly defined"){
		assert(oddNumbers == List(1,3,5))
	}

	test("sumDouble works as intended"){
		assert(sumDouble(List()) == 0)
		assert(sumDouble(List(1)) == 2)
		assert(sumDouble(List(-1)) == -2)
		assert(sumDouble(List(1,2,3)) == 12)
		assert(sumDouble(List(1,2,3, -1)) == 10)
	}

	test("removeZeroes works as intended"){
		assert(removeZeroes(List(0,0,0)) == List())
		assert(removeZeroes(List(0,0,0, -1)) == List(-1))
		assert(removeZeroes(List()) == List())
		assert(removeZeroes(List(54,35)) == List(54,35))
		assert(removeZeroes(List(1,0,0,1,0)) == List(1,1))
		assert(removeZeroes(List(1,0,50,54,0,1,0,60)) == List(1,50,54,1,60))
	}

	test("countEvens works as intended"){
		assert(countEvens(List()) == 0)
		assert(countEvens(List(1,2,3,4,6)) == 3)
		assert(countEvens(List(0,0,0)) == 3)
		assert(countEvens(List(1,-2,3,4,-6, 7,-9)) == 3)
	}

	test("removeAlternating works as intended"){
		assert(removeAlternating(List("A", "B", "C")) == List("A", "C"))
		assert(removeAlternating(List("A", "B", "P", "D")) == List("A", "P"))
		assert(removeAlternating(List()) == List())
		assert(removeAlternating(List("A")) == List("A"))
	}

	test("isAscending works as intended"){
		assert(isAscending(List()) == true)
		assert(isAscending(List(30)) == true)
		assert(isAscending(List(30, 1)) == false)
		assert(isAscending(List(1,2,3,4)) == true)
		assert(isAscending(List(-1, 1,2)) == true)
		assert(isAscending(List(1,5,3,4)) == false)
	}

	test("addSub works as intended"){
		assert(addSub(List(1,2,3,4,5)) == 3)
		assert(addSub(List(1,-2,3,4,5)) == 7)
		assert(addSub(List(1,3,5,8,0,3,10)) == 2)
		assert(addSub(List(0,0,0,0,0,0,0)) == 0)
		assert(addSub(List(1,1)) == 0)
		assert(addSub(List()) == 0)
	}

	test("alternate works as intended"){
		assert(alternate(List(1,5), List(1,8)) == List(1,1,5,8))
		assert(alternate(List(1,5,7), List(2,8)) == List(1,2,5,8,7))
		assert(alternate(List(1,5,7), List(1,8,9,10,11)) == List(1,1,5,8,7,9,10,11))
		assert(alternate(List(1, 3, 5), List(2, 4, 6)) == List(1, 2, 3, 4, 5, 6))
		assert(alternate(List(), List(2, 4, 6)) == List(2,4,6))
		assert(alternate(List(1,2), List()) == List(1,2))
		assert(alternate(List(), List()) == List())
	}

	test("fromTo works as intended"){
		assert(fromTo(1,1) == List())
		assert(fromTo(-1,1) == List(-1,0))
		assert(fromTo(1,5) == List(1,2,3,4))
		assert(fromTo(5,6) == List(5))
	}

	test("insertedOrder works as intended"){
		assert(insertOrdered(5, List(1,3,7,9)) == List(1, 3, 5, 7, 9))
		assert(insertOrdered(1, List(1,3,7,9)) == List(1, 1, 3, 7, 9))
		assert(insertOrdered(1, List(8)) == List(1,8))
		assert(insertOrdered(1, List()) == List(1))
		assert(insertOrdered(-1, List(-5, -3)) == List(-5, -3, -1))
	}

	test("sort works as intended"){
		assert(sort(List(1,3,4,2)) == List(1,2,3,4))
		assert(sort(List(2,3,4,1)) == List(1,2,3,4))
		assert(sort(List(1,2,90)) == List(1,2,90))
		assert(sort(List(1)) == List(1))
		assert(sort(List()) == List())
		assert(sort(List(90,0,-1,1)) == List(-1,0,1,90))
	}
}