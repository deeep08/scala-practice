package db.s99

object FlattenP07 {
  def main(args: Array[String]): Unit = {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }

  def flatten(list: List[Any]): List[Any] = {
    list flatMap {
      case l: List[_] => flatten(l)
      case e => List(e)
    }
  }
}
