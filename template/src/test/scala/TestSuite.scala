// You may modify or delete this file
class TestSuite extends org.scalatest.FunSuite {
  import edu.umass.cs.CSV
  import Homework3._

  test("Have life expectancies from 1930 -- 2010") {
    assert(CSV.fromFile("cdc-life-expectancy.csv").map(x => x(0).toInt).reverse
           == 1930.to(2010))
  }

  test("yearIs test-1"){
  	val lst = List(List("1918", "China"), List("1920", "China"), List("1920", "Japan"))
  	assert(yearIs(lst, 1920) == List(List("1920", "China"), List("1920", "Japan")))
  }

  test("yearGT test-1"){
    val lst = List(List("1918", "China"), List("1920", "China"), List("1920", "Japan"))
    assert(yearGT(lst, 1922) == List())
    assert(yearGT(lst, 1918) == List(List("1920", "China"), List("1920", "Japan")))
    assert(yearGT(lst, 1916) == lst)
  }

  test("yearLT test-1"){
    val lst = List(List("1918", "China"), List("1920", "China"), List("1920", "Japan"))
    assert(yearLT(lst, 1920) == List(List("1918", "China")))
    assert(yearLT(lst, 1918) == List())
    assert(yearLT(lst, 1922) == lst)
  }

  test("onlyName test-1"){
    val lst = List(List("1918", "China"), List("1920", "China"), List("1920", "Japan"))
    assert(onlyName(lst, "China") == List(List("1918", "China"), List("1920", "China")))
    assert(onlyName(lst, "Japan") == List(List("1920", "Japan")))
    assert(onlyName(lst, "Hong Kong") == List())
  }

  test("count test-1"){
    val lst = List(List("1880","Mary","F","7065"), List("1880","Anna","F","2604"), List("1880","Emma","F","2003"),List("1880","Elizabeth","F","1939"),List("1880","Minnie","F","1746"),List("1880","Margaret","F","1578"))
    assert(count(lst) == 16935)  
  }

  test("mostPopular test-1"){
    val lst = List(List("1880","Mary","F","7065"), List("1880","Anna","F","2604"), List("1880","Emma","F","2003"),List("1880","Elizabeth","F","1939"),List("1880","Minnie","F","1746"),List("1880","Margaret","F","1578"))
    assert(mostPopular(lst) == ("Mary", 7065))
  }

  test("countBoysAndGirls test-1"){
    val lst = List(List("1880","Mary","F","7065"), List("1880","Anna","F","2604"), List("1880","Emma","F","2003"),List("1880","Elizabeth","F","1939"),List("1880","Minnie","F","1746"),List("1880","Margaret","F","1578"))
    assert(countGirlsAndBoys(lst) == (0, 16935))
    assert(countGirlsAndBoys(List("1990", "Kev", "M", "100") :: lst) == (100, 16935))
    assert(countGirlsAndBoys(List(List("1990", "Kev", "M", "100"))) == (100, 0))
  }

  test("unisexNames test-1"){
    val lst = List(List("1880","Mary","F","7065"), List("1880","Anna","F","2604"), List("1880","Mary","M","7065"), List("1880","Emma","F","2003"),List("1880","Elizabeth","F","1939"),List("1880","Minnie","F","1746"),List("1880","Margaret","F","1578"))
    assert(unisexNames(lst) == Set("Mary"))
  }

  test("expectedAlive test-1"){
    assert(expectedAlive("F", 1990, 2009) == true)
    assert(expectedAlive("F", 1930, 2009) == false)
    assert(expectedAlive("F", 1936, 2000) == true)
    assert(expectedAlive("M", 1936, 2000) == false)
    assert(expectedAlive("M", 1996, 2069) == true)
    assert(expectedAlive("M", 1996, 2070) == false)
  }

  test("estimatePopulation test-1"){
    val lst = List(List("1980","Mary","F","7065"), List("1990","Anna","F","2604"), List("1970","Emma","F","2003"),List("1983","Elizabeth","F","1939"),List("1990","Minnie","F","1746"),List("1989","Margaret","F","1578"))
    assert(estimatePopulation(lst, 2010) == 16935)
    assert(estimatePopulation(lst, 2090) == 0)
  }
}