
import hw.generics._
import ListFunctions._

class GenericsTest extends org.scalatest.FunSuite {
	test("filter test-1"){
		def isOne(x: Int): Boolean = if (x == 1) true else false
		val tree = Leaf().cons(1).cons(1).cons(2).cons(1)
		val t2 = Leaf().cons(3).cons(9)
		assert(filter(isOne, tree) == Leaf().cons(1).cons(1).cons(1))
		assert(filter(isOne, t2) == Leaf())
	}

	test("append test-1"){
		val t1 = Leaf().cons(1)
		val t2 = Leaf().cons(2)
		assert(append[Int,BinTree[Int]](t1,t2) == Node(Leaf(),2,Node(Leaf(),1,Leaf())))
		assert(Leaf().cons(1).cons(2) == Node(Leaf(),2,Node(Leaf(),1,Leaf())))
		assert(append[Int, BinTree[Int]](Leaf(), t1) == t1)		
		assert(append[Int, BinTree[Int]](t1, Leaf()) == t1)
	}

	test("sort test-1"){
		// assert(sort(tree) == Leaf().cons(OrdInt(10)).cons(OrdInt(5)).cons(OrdInt(2)).cons(OrdInt(-1)))
	}
}