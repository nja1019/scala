import Exercises._
import org.scalatest.FunSuite

class ExerciseTests extends FunSuite {
	// test("map over exercise1 works"){
	// 	assert(exercise1(List(Duck(), Duck(), Goose())) == List("dog food", "dog food", "pate"))
	// 	assert(exercise1(List(Duck(), Goose(), Goose())) == List("dog food", "pate", "pate"))
	// }

	test("fold method works"){
		assert(exercise3(List(Duck(), Duck(), Goose())) == 12)
		assert(exercise3(List(Goose(), Goose(), Goose(), Duck(), Goose())) == 41)
		assert(exercise3(List()) == 0)
	}
}