class TrivialTestSuite extends org.scalatest.FunSuite {

  test("The solution object must be defined") {
    val obj : MinimaxLike = Solution
  }

  test("hasPlayerWon test-1"){
  	val game1 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O))
  	assert(game1.hasPlayerWon(X) == false)
  	assert(game1.hasPlayerWon(O) == false)
    val game3 = Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, (0, 2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> X, (2,0) -> X, (2,1) -> O, (2, 2) -> O))
    assert(game3.hasPlayerWon(X) == false)
    val game4 = Solution.createGame(X, 4, Map())
    assert(game4.hasPlayerWon(X) == false)
    assert(game4.hasPlayerWon(O) == false)
    val game2 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O, (0,1) -> X))
    assert(game2.hasPlayerWon(X) == true)    
    assert(game2.hasPlayerWon(O) == false)
  }

  test("getWinner test-1"){
  	val game1 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O))
  	val game2 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O, (0,1) -> X))
    assert(game1.getWinner() == None)
    assert(game2.getWinner() == Some(X))
    val game4 = Solution.createGame(X, 4, Map())
    assert(game4.getWinner() == None)
    assert(game4.getWinner() == None)
  }

  test("isFinished test-1"){
    val game2 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O, (0,1) -> X))
    assert(game2.isFinished() == true)
    val game1 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O))
    assert(game1.isFinished() == false)
    val game3 = Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, (0, 2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> X, (2,0) -> X, (2,1) -> O, (2, 2) -> O))
    assert(game3.isFinished() == true)
    val game4 = Solution.createGame(X, 4, Map())
    assert(game4.isFinished() == false)
    assert(game4.isFinished() == false)
  }
  test("nextBoards test-1"){
    val game3 = Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, (0, 2) -> O, (1,0) -> O))
    assert(game3.nextBoards().size == 5)
    val myMap = Map((0,1) -> X, (0, 2) -> O, (1,0) -> O, (1,1) -> O, (1,2) -> X, (2,0) -> X, (2,1) -> O, (2, 2) -> O)
    val game4 = Solution.createGame(X, 3, myMap)
    val game5 = Solution.createGame(O, 3, myMap + ((0,0) -> X))
    assert(game4.nextBoards().size == 1)
    assert(game5.nextBoards() == Nil)
  }
  test("minimax test-1"){
    val game2 = Solution.createGame(X, 3, Map((0, 0) -> X, (0, 2) -> X, (2, 2) -> O, (0,1) -> X))
    val g3 = Solution.createGame(X, 3, Map())
    assert(Solution.minimax(game2) == Some(X))
    assert(Solution.minimax(g3) == None)
    val game3 = Solution.createGame(X, 3, Map((0, 0) -> X, (0,1) -> X, (0, 2) -> X, (1,0) -> O, (1,1) -> O, (1,2) -> X, (2,0) -> X, (2,1) -> O, (2, 2) -> O))
    assert(Solution.minimax(game3) == Some(X))

  }

}