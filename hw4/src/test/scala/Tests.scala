class Tests extends org.scalatest.FunSuite {

  import FunctionalDataStructures._

  def fromList[A](lst: List[A]): JoinList[A] = lst match {
    case Nil => Empty()
    case List(x) => Singleton(x)
    case _  => {
      val len = lst.length
      val (lhs, rhs) = lst.splitAt(len / 2)
      Join(fromList(lhs), fromList(rhs), len)
    }
  }

  def toList[A](lst: JoinList[A]): List[A] = lst match {
    case Empty() => Nil
    case Singleton(x) => List(x)
    case Join(lst1, lst2, _) => toList(lst1) ++ toList(lst2)
  }

  def mapTest[A, B](f: A => B, alist: List[A]): Boolean = {
    toList(map(f, fromList(alist))) == alist.map(f)
  }

  // def restTest[A](jlist: JoinList[A]): Boolean = {
  //   val alist = toList(jlist)
  //   rest(alist) match {
  //     case None => alist == Nil
  //     case Some(jlistRest) => jlistRest == alist.rest
  //   }
  // }

  test("map test-1"){
    val lst = List(1, 2, 4)
    def toString(x: Any): String = x.toString
    assert(mapTest(toString, lst) == true)
  }

  test("filter test-1"){
    def isOne(x: Any): Boolean = if (x == 1) true else false
    val lst = List(1,2,3,4,5,6,7,8,9)
    assert(filter(isOne, Empty()) == Empty())
    assert(filter(isOne, Singleton(1)) == Singleton(1))
    assert(toList(filter(isOne, Join(Join(Singleton(1), Singleton(1), 2), Join(Singleton(3), Singleton(1), 2), 4))) == toList(Join(Join(Singleton(1), Singleton(1), 2), Join(Empty(), Singleton(1), 1), 3)))
  }

  test("rest test-1"){
    assert(rest(Join(Empty(), Singleton(1), 1)) == Some(Empty()))
  }

  test("nth test-1"){
    
  }
  
  test("enqueue test-1"){
    val myQ = Queue(List(1,2), List(3,3))
    assert(enqueue(4, myQ) == Queue(List(1,2), List(4,3,3)))
  }
  test("dequeue test-1"){
    val myQ = Queue(List(1,2), List(3,3))
    val myQ2 = Queue(List(), List(4,3))
    assert(dequeue(myQ) == Option(1, Queue(List(2), List(3,3))))
    assert(dequeue(myQ2) == Option(3, Queue(List(), List(4))))
  }
}