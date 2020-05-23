package db.s99

import scala.annotation.tailrec

// P17 (*) Split a list into two parts.
//     The length of the first part is given.  Use a Tuple for your result.
//
//     Example:
//     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
object ProblemP17 {
  def main(args: Array[String]): Unit = {
    println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

    val split4 = (split[Symbol] _).curried(4)

    println(split4(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

    println(splitUsingTakeNDrop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(splitUsingRecursion(6, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(splitTailRecursion(6, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

    println(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).splitAt(6))

  }

  def split[A](index: Int, list: List[A]): (List[A], List[A]) = {
    val tmp = list.zipWithIndex.span(t => t._2 < index)

    (tmp._1 map(_._1), tmp._2 map(_._1))
  }

  def splitUsingTakeNDrop[A](index: Int, list: List[A]): (List[A], List[A]) = {
    (list.take(index), list.drop(index))
  }

  def splitUsingRecursion[A](index: Int, list: List[A]): (List[A], List[A]) = {
    (index, list) match {
      case (_, Nil) => (Nil, Nil)
      case (0, list) =>  (Nil, list)
      case (i, head::tail) =>  val (t1, t2) = splitUsingRecursion(i - 1, tail)
        (head :: t1, t2)
    }
  }

  def splitTailRecursion[A](index: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitHelper(index: Int, list: List[A], first: List[A]): (List[A], List[A]) = {
      (index, list) match {
        case (_, Nil) => (first.reverse, Nil)
        case (0, rem) => (first.reverse, rem)
        case (i, head::tail) => splitHelper(i-1, tail, head :: first)
      }
    }

    splitHelper(index, list, Nil)
  }
}
