object Homework2{
    //Done
	def map2[A,B,C](f: (A,B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1,lst2) match{
		case (Nil, Nil) => Nil
		case(Nil, _) => Nil
		case (_,Nil) => Nil
		case (h1 :: t1, h2 :: t2)  => f(h1, h2) :: map2[A,B,C](f, t1, t2)
	}
	//Done
	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1,lst2) match{
		case (Nil, Nil) => Nil
		case(Nil, _) => Nil
		case (_,Nil) => Nil
		case (x :: rest1, y :: rest2) => {
			val newEntry = (x, y)
			newEntry :: zip(rest1, rest2)
		}
	}

	//Done
	//Flattens a nested list into 1 list
	def flatten[A](lst: List[List[A]]): List[A] = lst match{
 		case Nil => Nil
 		case n :: Nil => n
 		case n :: rest => append(flattenHelper(n), flatten(rest))
 	}
 	//Allows you to access nested lists
 	def flattenHelper[A](lst1: List[A]): List[A] = lst1 match{
		case Nil => Nil
		case n :: rest1 => n :: flattenHelper(rest1)
 	}
 	//Appends nested lists
 	def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match{
 		case (Nil, Nil) => Nil
 		case (Nil, n2 :: rest) => n2 :: append(Nil, rest)
 		case (n :: rest, _) => n :: append(rest, lst2)
 	}
 	//Done...Write more tests
 	def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match{
 		case Nil => Nil
 		case n :: rest => flatten(n)
 	}

 	//Done
 	def buildList[A](length: Int, f: Int => A): List[A] = length match{
 		case 0 => Nil
 		case x => append(buildList(length - 1, f), List(f(x - 1)))
 	}

 	//Done
 	def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = partitionHelp(f, lst, List(), List())
 	//Helper
 	def partitionHelp[A](f: A => Boolean, orig: List[A], trueList: List[A], falseList: List[A]): (List[A], List[A]) = orig match{
 		case Nil => (trueList, falseList)
 		case n :: rest =>{
 			if(f(n)){
 				val lst1 = append(trueList, List(n))
 				partitionHelp(f, rest, lst1, falseList)
 			}
 			else{
 				val lst1 = append(falseList, List(n))
 				partitionHelp(f, rest, trueList, lst1)
 			} 
 		}
 	}
 	//Done
 	def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match{
 		case Nil => Nil
 		case n :: rest => append(f(n), mapList[A,B](rest,f))
 	}

}




