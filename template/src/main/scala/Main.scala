object Homework3 {

  import edu.umass.cs.CSV


  // WARNING: this may take a very long time. Cut the file or work with a
  // small, made-up dataset if you have trouble.
  // val allBirths = CSV.fromFile("ssa-births.csv")

  val lifeExpectancy = CSV.fromFile("cdc-life-expectancy.csv")

  /** Restrict the data to the year `year`. */
  def yearIs(data: List[List[String]], year: Int): List[List[String]] = {
    data.filter(lst => lst(0).toInt == year)
  }

  /** Restrict the data to years greater than `bound`. */
  def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
    data.filter(lst => lst(0).toInt > bound)
  }

  /** Restrict the data to years less than `bound` */
  def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
    data.filter(lst => lst(0).toInt < bound)
  }

  /** Restrict the data to the name `name`. */
  def onlyName(data: List[List[String]], name: String): List[List[String]] = {
    data.filter(lst => lst(1).equals(name))
  }

  /** Calculate the most popular name and the number of children born with
      that name. */
  def mostPopular(data: List[List[String]]): (String, Int) = mostPopHelper(data, "", 0)

  def mostPopHelper(data: List[List[String]], name: String, count: Int): (String, Int) = data match{
    case Nil => (name, count)
    case head :: rest =>{
      if(head(3).toInt > count){
        val newCount = head(3).toInt
        val newName = head(1)
        mostPopHelper(rest, newName, newCount)
      }
      else mostPopHelper(rest, name, count)
    }
  }

  /** Calculate the number of children born in the given dataset. */
  def count(data: List[List[String]]): Int = data match{
    case Nil => 0
    case head :: rest => head(3).toInt + count(rest)
  }

  /** Produce a tuple with the number of girls and boys respectively. */
  def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = countGandBHelper(data, 0, 0)

  def countGandBHelper(data: List[List[String]], boyCount: Int, girlCount: Int): (Int, Int) = data match{
    case Nil => (boyCount,girlCount)
    case head :: rest =>{
      if(head(2).equals("F")){
        val newGirlCount = head(3).toInt + girlCount
        countGandBHelper(rest, boyCount, newGirlCount)
      }
      else{
        val newBoyCount = head(3).toInt + boyCount
        countGandBHelper(rest, newBoyCount, girlCount)
      }
    }
  }
  
  /** Calculate the set of names that are given to both girls and boys. */
  def unisexNames(data: List[List[String]]): Set[String] = {
    val boyList = getBoys(data, List())
    val girlList = getGirls(data, List())
    boyList.intersect(girlList)
  }
  
  def getBoys(data: List[List[String]], boyList: List[String]): Set[String] = {
    data match{
      case Nil => boyList.toSet
      case h :: rest =>{
        if(h(2).equals("M")) getBoys(rest, h(1) :: boyList)
        else getBoys(rest, boyList)
      }
    }
  }
  def getGirls(data: List[List[String]], girlList: List[String]): Set[String] = {
    data match{
      case Nil => girlList.toSet
      case h :: rest =>{
        if(h(2).equals("F")) getGirls(rest, h(1) :: girlList)
        else getGirls(rest, girlList)
      }
    }
  }

  /** Determine if a person with the specified `gender` (either "M" or "F") who
      was born in `birthYear` is expected to be alive, according to the CDC
      life-expectancy data.

      If `currentYear` is the last year the person is estimated to be alive, be
      optimistic and produce `true`.

      The CDC data only ranges from 1930 -- 2010. Therefore, assume that
      `birthYear` is in this range too. */
  def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
    if(birthYear > 2010 || birthYear < 1930) false
    if(gender.equals("F")){
      val myEntry = lifeExpectancy.filter(x => x(0).toInt == birthYear)
      if(myEntry(0)(2).toInt < (currentYear - birthYear)) false
      else true
    }
    else{
      val myEntry = lifeExpectancy.filter(x => x(0).toInt == birthYear)
      if(myEntry(0)(1).toInt < (currentYear - birthYear)) false
      else true
    }
  }

  /** Estimate how many people from `data` will be alive in `year`. */
  def estimatePopulation(data: List[List[String]], year: Int): Int ={
    val myList = data.filter(x => expectedAlive(x(2), x(0).toInt, year) == true)
    count(myList)
  }
}