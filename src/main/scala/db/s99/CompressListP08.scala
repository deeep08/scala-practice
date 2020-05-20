package db.s99

import scala.annotation.tailrec

object CompressListP08 {
  def main(args: Array[String]): Unit = {
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(compressTail(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(compressFoldRight(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(compressFoldLeft(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }

  def compress[A](list: List[A]): List[A] = {
    list match {
      case head +: tail => head +: compress(tail.dropWhile(_ == head))
      case Nil => Nil
    }
  }

  def compressTail[A](list: List[A]): List[A] = {
    @tailrec
    def com[A](ls: List[A], res: List[A]): List[A] = {
      ls match {
        case Nil => res
        case head :: tail => com(tail.dropWhile(_ == head), res :+ head)
      }
    }
    com(list, Nil)
  }

  def compressFoldRight[A](list: List[A]) : List[A] = {
    list.foldRight(List.empty[A]) {
      (a, b) =>
        if(b.isEmpty || a != b.head) a :: b
        else b
    }
  }

  def compressFoldLeft[A](list: List[A]) = {
    list.foldLeft(List.empty[A]) {
      (res, elem) => if(res.isEmpty || res.last != elem) res :+ elem else res
    }
  }
}
