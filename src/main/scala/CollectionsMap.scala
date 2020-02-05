class CollectionsMap {
  def sumConditional(map: Map[String, Int], str: String): Int = {
    map.foldLeft(0)((sumOfAge: Int, name: (String,Int)) => if(name._1.contains(str)) sumOfAge + name._2 else sumOfAge + 0)
  }

  private def changeList(list: List[String], changeWith: String, resultantList: List[String]): List[String] = {
    list match {
      case Nil => resultantList
      case firstString :: rest => changeList(rest, changeWith, resultantList :+ firstString(0) + changeWith)
    }
  }

  def changeStringValuesInList(map: Map[Int, List[String]]): Map[Int, List[String]] = {
    map.foldLeft(Map[Int, List[String]]()) {(resultantMap: Map[Int, List[String]], mapElement: (Int, List[String])) =>
        if(mapElement._1 % 2 == 0) {
          val updatedList = changeList(mapElement._2, "even", List.empty[String])
          resultantMap + (mapElement._1 -> updatedList)
          }
        else {
          val updatedList = changeList(mapElement._2, "odd", List.empty[String])
          resultantMap + (mapElement._1 -> updatedList)
        }
    }
  }

  def last(list: List[Int]): Int = {
    list.foldLeft(0)((lastElement: Int, currentElement: Int) => currentElement)
  }

}

object CollectionsMap extends App {

  val collectionsMap = new CollectionsMap()

  val map1 = Map("anurag" -> 24, "daniel" -> 23, "anushka" -> 30)
  val str = "anu"

  println(collectionsMap.sumConditional(map1, str))

  val map2 = Map(
    1 -> List("Sunil", "Laxmi"),
    2 -> List("Bhavya", "Sangeeta"),
    3 -> List("Arun", "Sushmita"),
    4 -> List("Jamwant")
  )

  println(collectionsMap.changeStringValuesInList(map2))

  println(collectionsMap.last(List(1, 1, 2, 3, 5, 8)))
}

