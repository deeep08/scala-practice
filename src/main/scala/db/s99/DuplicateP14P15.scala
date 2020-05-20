package db.s99

// P14 (*) Duplicate the elements of a list.
//     Example:
//     scala> duplicate(List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
object DuplicateP14P15 {
  def main(args: Array[String]): Unit = {
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

    val duplicateCurried = (duplicateN[Any] _).curried
    val duplicate4 = duplicateCurried(4)

    println(duplicate4(List('a, 'b, 'c, 'c, 'd)))
  }

  def duplicate[A](list: List[A]): List[A] = {
    list flatMap {
      x => List(x, x)
    }
  }

  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    list flatMap { e => List.fill(n)(e) }
  }
}
