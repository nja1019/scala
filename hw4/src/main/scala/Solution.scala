object FunctionalDataStructures {

  //
  // Part 1. Persistent Queues
  //
  //Done
  def enqueue[A](elt: A, q: Queue[A]): Queue[A] = q match{
    case Queue(Nil, Nil) => Queue(Nil, List(elt))
    case Queue(fr, bk) => Queue(fr, elt :: bk)
  }

  //Done
  def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = q match{
    case Queue(Nil, Nil) => None
    case Queue(Nil, bk) => Some((bk(bk.size - 1), Queue(Nil, bk.dropRight(1))))
    case Queue(fr, bk) => Some((fr(0), Queue(fr.drop(1), bk)))
  }


  //
  // Part 2. Join Lists
  //

  //BINARY SEARCH IS USEFUL
  def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = {
    maxHelp(lst, compare, first(lst).get)
  }

  def maxHelp[A](lst: JoinList[A], compare: (A, A) => Boolean, max: A): Option[A] = {
    if(lst == Empty()) Some(max)
    if(compare(first(lst).get, max)) maxHelp(rest(lst).get, compare, first(lst).get)
    else{
      maxHelp(rest(lst).get, compare, max)
    }
  }
 

  def first[A](lst: JoinList[A]): Option[A] = lst match{
    case Empty() => None
    case Singleton(x) => Some(x)
    case Join(x, y, z) =>{
      if(x == Empty()) first(y)
      else first(x)
    }
  }

  def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match{
    case Empty() => None
    case Singleton(x) => Some(Empty())
    case Join(x, y, z) => {
      if(x.size == 0){
        rest(y)
      }
      else if(x.size <= 1){
        Some(y)
      }
      else Some(Join(rest(x).get, y, z - 1))
    }
  }

  def nth[A](lst: JoinList[A], n: Int): Option[A] = {
    if(n == 0) first(lst)
    else{
      nth(rest(lst).get, n - 1)
    }
  }

  def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match{
    case Empty() => Empty()
    case Singleton(x) => Singleton(f(x))
    case Join(x, y, z) =>{
      Join(map(f, x), map(f, y), z)
    } 
  }

  def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match{
    case Empty() => Empty()
    case Singleton(x) =>{
      if(pred(x)) Singleton(x)
      else Empty()
    }
    case Join(x, y, z) => Join(filter(pred, x), filter(pred, y), z)
  }

}