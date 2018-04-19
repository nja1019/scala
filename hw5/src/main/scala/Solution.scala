class Game(turn: Player, mtrX: Matrix[Option[Player]]) extends GameLike[Game] {

  def isFinished(): Boolean = isFinishedHelper(mtrX.toList((a,b,c) => c))

  def isFinishedHelper(lst: List[Option[Player]]): Boolean = {
    if(hasPlayerWon(X) || hasPlayerWon(O)) true
    else if(isDraw(lst)) true
    else false
  }

  def isDraw(lst: List[Option[Player]]): Boolean = lst match{
    case Nil => false
    case None :: _ => false
    case Some(x) :: Nil => true
    case Some(x) :: rest => isDraw(rest)
  }

  def hasPlayerWon(p: Player): Boolean = {
  	val possibleWins = mtrX.mainDiagonal :: mtrX.antiDiagonal :: mtrX.rows ::: mtrX.cols
  	possibleWins.exists(row => row.forall(pos => pos == Some(p)))

  }

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = {
    if(hasPlayerWon(X)) Some(X)
    else if(hasPlayerWon(O)) Some(O)
    else None
  }

  def nextBoards(): List[Game] = {
    val openSpots = whereAmIEmpty(mtrX.toList((a,b,c) =>c), 0)
    val newBoards = nextHelp(openSpots, Nil)
    newBoards
  }

  def whereAmIEmpty(lst: List[Option[Player]], position: Int): List[Int] = lst match {
    case Nil => Nil
    case Some(x) :: rest => whereAmIEmpty(rest, position + 1)
    case None :: rest => position :: whereAmIEmpty(rest, position + 1)
  }

  def nextHelp(openSpots: List[Int], myList: List[Game]): List[Game] = openSpots match {
    case Nil => myList
    case x :: rest =>{
      val newMat = mtrX
      val myGame = new Game(nextTurn(turn), newMat.set(x / mtrX.rows.size, x % mtrX.rows.size, Some(turn)))
      nextHelp(rest, myGame :: myList)
    }  
  }

    //For Testing
  def getMatrix(): Matrix[Option[Player]] = mtrX
  def getTurn(): Player = turn
  def nextTurn(turnNow: Player): Player = if(turnNow == X) O else X
}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	new Game(turn, Matrix.fromMap(dim, None, board.mapValues(p => Some(p))))
  }

  def minimax(board: Game): Option[Player] = {
    if(board.getTurn() == X){
      if(board.hasPlayerWon(X)) Some(X)
      else if(board.isDraw(board.getMatrix().toList((a,b,c) => c))) None
      else{
        val games = board.nextBoards()
        games match{
          case Nil => None
          case n :: rest =>{
            if(minimax(n) == Some(X)) Some(X)
            else if(minimax(n) == None) None
            else if(minimax(rest.head) == Some(X)) Some(X)
            else if(minimax(rest.head) == None) None
            else Some(O)
          }
        }
      }
    }
    else{
      if(board.hasPlayerWon(O)) Some(O)
      else if(board.isDraw(board.getMatrix().toList((a,b,c) => c))) None
      else{
        val games = board.nextBoards()
        games match{
          case Nil => None
          case n :: rest =>{
            if(minimax(n) == Some(O)) Some(O)
            else if(minimax(n) == None) None
            else if(minimax(rest.head) == Some(O)) Some(O)
            else if(minimax(rest.head) == None) None
            else Some(X)
          }
        }
      }
    }
  }
}