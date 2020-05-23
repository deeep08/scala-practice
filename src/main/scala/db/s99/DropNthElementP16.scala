package db.s99

import scala.annotation.tailrec

// P16 (**) Drop every Nth element from a list.
//     Example:
//     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
object DropNthElementP16 {
  def main(args: Array[String]): Unit = {
    println(dropUsingZip(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(drop(1, List('a)))
    println(dropUsingFold(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  def dropUsingZip[A](index: Int, list: List[A]): List[A] = {
    list.zipWithIndex.filter {
      t => (t._2 + 1) % index != 0
    }.map(_._1)
  }

  def dropUsingFold[A](index: Int, list: List[A]): List[A] = {
    val foldOperation: (List[(Int, A)], A) => List[(Int, A)] = (acc, ele) => {
      if(acc.isEmpty) acc :+ (1, ele)
      else acc :+ (acc.last._1+1, ele)
    }

    list.foldLeft(List.empty[(Int, A)])(foldOperation).filter(x => x._1%index != 0).map(_._2)
  }

  def drop[A](index: Int, list: List[A]): List[A] = {
    @tailrec
    def dropHelper(count: Int, list: List[A], res: List[A]): List[A] = {
      list match {
        case Nil => res
        case _ :: tail if count == index => dropHelper(1, tail, res)
        case head :: tail => dropHelper(count+1, tail, res:+head)
      }
    }

    dropHelper(1, list, Nil)
  }
}
