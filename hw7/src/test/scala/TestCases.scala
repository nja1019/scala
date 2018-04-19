class TrivialTestSuite extends org . scalatest . FunSuite {
	test (" The solution object must be defined " ) {
		val obj : hw . sudoku . SudokuLike = Solution
	}


	test("parse test-1"){
		val game1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val g1 = Solution.parse(game1)
		val g2 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
		val c1 = "296318574584972613713645289625897341931426857478531926167253498859764132342189765"
		val parseC1 = Solution.parse(c1)
		assert(parseC1.toStringOver(parseC1) == c1)
		println(g1)
	}

	test("valueAt test-1"){
		val game1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val g1 = Solution.parse(game1)
		assert(g1.valueAt(0,1) == Some(4))
		assert(g1.valueAt(0,2) == Some(3))
		assert(g1.valueAt(0,0) == None)
		assert(g1.valueAt(8,8) == None)
	}

	test("isSolved test-1"){
		val game1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val g1 = Solution.parse(game1)
		val c1 = "296318574584972613713645289625897341931426857478531926167253498859764132342189765"
		val parseC1 = Solution.parse(c1)
		assert(g1.isSolved == false)
		assert(parseC1.isSolved == true)
	}

	test("isUnsolvable test-1"){
		val game1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val g1 = Solution.parse(game1)
		val c1 = "296318574584972613713645289625897341931426857478531926167253498859764132342189765"
		val parseC1 = Solution.parse(c1)
		assert(g1.isUnsolvable == false)
		assert(parseC1.isUnsolvable == false)
	}

	test("toString test-1"){
		val game1 = ".43......6.............1........4.7....6......1.2........5.............5.3....71."
		val g1 = Solution.parse(game1)
		assert(g1.toStringOver(g1) == game1)
	}

	test("place test-1"){
		val game1 = "................................................................................."
		val g1 = Solution.parse(game1)
		val g2 = g1.place(0,0,1)
		val g3 = g2.place(0,1,2)
		assert(g2.valueAt(0,0) == Some(1))
		assert(g3.valueAt(0,1) == Some(2))
	}

	test("nextStates test-1"){
		val game1 = "......9.7...42.18....7.5.261..9.4....5.....4....5.7..992.1.8....34.59...5.7......"
		val g1 = Solution.parse(game1)
		val g2 = g1.nextStates
		println(g2.length)
		
	}

}