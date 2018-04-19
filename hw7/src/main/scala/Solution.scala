import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = parseHelper(str, 0, emptyBoard())

  def parseHelper(str: String, counter: Int, myBoard: Board): Board = {
    if(str.isEmpty || counter == 81) myBoard
    else str(0) match {
      case '.' =>{
        val col = counter % 9 
        val row = counter / 9
        // What about when you know there is only 1 value in the '.' space? This can be optimized
        if(myBoard.available(row,col).length == 1){
          val newBoard = new Board(updatePeers(peers(row, col), myBoard.available, myBoard.available(row, col)(0)))
          parseHelper(str.substring(1), counter + 1, newBoard)
        }else parseHelper(str.substring(1), counter + 1, myBoard)
      }
      case x => {
        val col = counter % 9 
        val row = counter / 9
        val newBoard = myBoard.place(row, col, x.asDigit)
        parseHelper(str.substring(1), counter + 1, newBoard)
      }
    }
  }

  def updatePeers(peers: List[(Int, Int)], gameMap: Map[(Int, Int), List[Int]], removeMe: Int): Map[(Int, Int), List[Int]] = peers match{
    case Nil => gameMap
    case x :: rest =>{
      val newMap = gameMap + (x -> gameMap(x).filterNot(elem => elem == removeMe))
      updatePeers(rest, newMap, removeMe)
    }
  }
  
  def emptyBoard(): Board = {
    new Board(Map((0.to(8).flatMap{r => 0.to(8).map{c => ((r,c) -> List(1,2,3,4,5,6,7,8,9))}}):_*))
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = peersTbl((row,col))
  
  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map { r => (r, col) }    
    val colPeers = 0.to(8).map { c => (row, c) }
    val boxRow: Int = (row / 3) * 3
    val boxCol: Int = (col / 3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap {
      r => boxCol.to(boxCol + 2).map {
        c => (r,c) 
      }
    }
    (rowPeers ++ colPeers ++ boxPeers).filterNot{
      case (r,c) => r == row && col == c 
    }.toList.distinct
  }
  val peersTbl = Map((0.to(8).flatMap {
    r => 0.to(8).map {
      c => ((r,c) -> calcPeers (r,c))
    }
  }) :_*)

}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {  

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }


  def valueAt(row: Int, col: Int): Option[Int] = available(row, col) match{
    case x :: Nil => Some(x)
    case _ => None
  }

  def isSolved(): Boolean = solveHelp(0, 0)

  def solveHelp(row: Int, col: Int): Boolean ={
    if(row < 9){
      if(col == 8){
        if(valueAt(row, col) == None) false
        else solveHelp(row + 1, 0)
      }
      else{
        if(valueAt(row, col) == None) false
        else solveHelp(row, col + 1)
      }
    }else true
  }

  def isUnsolvable(): Boolean = {
    if(isSolved) false
    else isUnsolvableHelp(0,0)
  }
  def isUnsolvableHelp(row: Int, col: Int): Boolean ={
    if(row < 9){
      if(col == 8){
        if(available(row, col) == Nil) true
        else isUnsolvableHelp(row + 1, 0)
      }
      else{
        if(available(row, col) == Nil) true
        else isUnsolvableHelp(row, col + 1)
      }
    }else false
  }

  //DONE
  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val newMap = available + ((row, col) -> List(value))
    val myPeers = Solution.peers(row, col)
    val updatedMap = placeHelper(myPeers, newMap, value)
    new Board(updatedMap)
  }

  def placeHelper(peers: List[(Int, Int)], myMap: Map[(Int, Int), List[Int]], removeMe: Int): Map[(Int, Int), List[Int]] = peers match {
    case Nil => myMap
    case x :: rest => {
      val newMap = myMap + (x -> myMap(x).filterNot(elem => elem == removeMe))
      if(newMap(x).length == 1){
        val row = x._1
        val col = x._2
        val myPeers = Solution.peers(row, col)
        val updatedMap = Solution.updatePeers(myPeers, newMap, newMap(x)(0))
        placeHelper(rest, updatedMap, removeMe)
      }
      else placeHelper(rest, newMap, removeMe)
    }
  }

  //For testing, from Piazza
  def toStringOver(myBoard: Board): String = {
    val keyValues = myBoard.available.keys.toList.sorted
    toStringHelper(keyValues, "", myBoard)
  }

  def toStringHelper(keyValues: List[(Int, Int)], myString: String, myBoard: Board): String = keyValues match {
    case Nil => myString+""
    case hd :: tl => {
      if(myBoard.available(hd).length > 1){
        toStringHelper(tl, myString+'.', myBoard)
      }
      else toStringHelper(tl, myString+myBoard.available(hd)(0).toString, myBoard)
    }
  }

  def getAvailable(): Map[(Int, Int), List[Int]] = available

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) {
      List()
    }else nextHelp(0,0).sortWith(_.available.values.flatten.size < _.available.values.flatten.size)
  }

  def nextHelp(row: Int, col: Int): List[Board] = {
    if(row < 9){
      if(col == 8){
        val availableMoves = available(row, col)
        if(availableMoves.length == 1){
          nextHelp(row + 1, 0)
       }
        else{
          val cellList = nextMoves(availableMoves, row, col)
          nextHelp(row + 1, 0) ::: cellList
        }
      }
      else{
        val availableMoves = available(row, col)
        if(availableMoves.length == 1) nextHelp(row, col + 1)
        else{
          val cellList = nextMoves(availableMoves, row, col)
          nextHelp(row, col + 1) ::: cellList
        }
      }
    }
    else Nil
  }

  def nextMoves(possibleMoves: List[Int], row: Int, col: Int): List[Board] = possibleMoves match {
    case Nil => Nil
    case hd :: tl =>{
      if(place(row, col, hd).isUnsolvable) nextMoves(tl, row, col)
      else place(row, col, hd) :: nextMoves(tl, row, col)
    }
  }

  def solve(): Option[Board] = {
    if(isSolved) Some(this)
    else if(isUnsolvable)  None
    else{
      solveHelp(nextStates)
    }
  }

  def solveHelp(lst: List[Board]): Option[Board] = lst match {
    case Nil => None
    case hd :: rest => {
      if(hd.solve != None)hd.solve
      else solveHelp(hd.nextStates ::: rest)
    }
  }
}