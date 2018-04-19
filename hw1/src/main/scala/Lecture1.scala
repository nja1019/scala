object Lecture1{
	val oddNumbers = 1 :: 3 :: 5 :: Nil
	//Done
	def sumDouble(lst: List[Int]): Int = lst match{
		case Nil => 0
		case n :: rest => n*2 + sumDouble(rest)
	}
	//Done
	def removeZeroes(lst: List[Int]): List[Int] = lst match{
		case Nil => Nil
		case n :: rest =>{
			if (n == 0) removeZeroes(rest)
			else n :: removeZeroes(rest)
		}
	}
	//Done
	def countEvens(lst: List[Int]): Int = countEvensHelper(lst, 0)
	//Helper
	def countEvensHelper(lst: List[Int], count: Int): Int = lst match{
		case Nil => count
		case n :: rest =>{
			if(n % 2 == 0) countEvensHelper(rest, count + 1)
			else countEvensHelper(rest, count)
		}
	}
	//Done
	def removeAlternating(lst: List[String]): List[String] = removeAlternatingHelper(lst, 0)
	//Helper
	def removeAlternatingHelper(lst: List[String], position: Int): List[String] = lst match {
		case Nil => Nil
		case n :: rest => {
			if(position % 2 == 0) n :: removeAlternatingHelper(rest, position + 1)
			else removeAlternatingHelper(rest, position + 1)
		}
	}
	//Done
	def isAscending(lst: List[Int]): Boolean = lst match{
		case Nil => true
		case n :: Nil => true
		case n :: rest => isAscendingHelper(rest, lst(0))
	}
	//Helper
	def isAscendingHelper(lst: List[Int], previous: Int): Boolean = lst match{
		case Nil => true
		case n :: rest =>{
			if(n >= previous){
				val newPrevious = n
				isAscendingHelper(rest, newPrevious)
			} 
			else false
		}
	}
	//Done
	def addSub(lst: List[Int]): Int = addSubHelper(lst, 0, 0)
	//Helper
	def addSubHelper(lst: List[Int], total: Int, position: Int): Int = lst match{
		case Nil => 0
		case n :: Nil =>{
			if(position % 2 == 0) total + n
			else total - n
		}
		case n :: rest =>{
			if(position % 2 == 0){
				val newTotal = total + n
				addSubHelper(rest, newTotal, position + 1)
			}
			else{
				val newTotal = total - n
				addSubHelper(rest, newTotal, position + 1)
			}
		}
	}
	//Done
	def alternate(lst1: List[Int], lst2: List[Int]): List[Int] = (lst1, lst2) match{
		case (Nil, Nil) => Nil
		case (n :: rest1, Nil) => n :: alternate(rest1, Nil)
		case (Nil, n :: rest2) => n :: alternate(Nil, rest2)
		case (n :: rest1, m :: rest2) => n :: m :: alternate(rest1, rest2)
	}
	//Done
	def fromTo(first: Int, last: Int): List[Int] = {
		val lst = List()
		val difference = last - first
		if(difference > 0) fromToHelper(first, last, last - 1, lst)
		else Nil
	}
	//Helper
	def fromToHelper(first: Int, last: Int, numberToAdd: Int, lst: List[Int]): List[Int] = {
		if(numberToAdd == first){
			val newLst = numberToAdd :: lst
			newLst
		}
		else{
			val newLst = numberToAdd :: lst
			fromToHelper(first, last, numberToAdd - 1, newLst)
		}
	}
	//Done
	def insertOrdered(n: Int, lst: List[Int]): List[Int] = lst match{
		//Assuming the list is ascending, per the directions
		//If its an empty list
		case Nil => List(n)
		//If n is less than newNumber, prepend n and newNumber in the correct order onto the front of the "rest" of the list
		case m :: rest if(n < m) => n :: m :: rest
		//Else, prepend n's onto the result of inserted newNumber in the rest of the list
		case m :: rest => m :: insertOrdered(n, rest)
	}
	//Do insertOrdered first
	def sort(lst: List[Int]): List[Int] = lst match{
		case Nil => Nil
		case n :: rest => insertOrdered(n, sort(rest))
	}
}