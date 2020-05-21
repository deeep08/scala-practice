package db.s99

// P18 (**) Extract a slice from a list.
//     Given two indices, I and K, the slice is the list containing the elements
//     from and including the Ith element up to but not including the Kth
//     element of the original list.  Start counting the elements with 0.
//
//     Example:
//     scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('d, 'e, 'f, 'g)
object ProblemP18 {
  def main(args: Array[String]): Unit = {
    println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(sliceWithRecursion(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(sliceWithBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
    list.zipWithIndex.filter {
      x => x._2 >= start && x._2 < end
    }.map(_._1)
  }

  def sliceWithRecursion[A](start: Int, end: Int, list: List[A]): List[A] = {
    (start, end, list) match {
      case (_, _, Nil) => Nil
      case (_, e, _) if e <= 0 => Nil
      case (0, e, head::tail) if e >= 0 => head::slice(0, e-1, tail)
      case (s, e, _::tail) if s > 0 => slice(s-1, e-1, tail)
    }
  }

  def sliceWithBuiltin[A](start: Int, end: Int, list: List[A]): List[A] = {
    list drop start take (end - (start max 0))
  }
}
