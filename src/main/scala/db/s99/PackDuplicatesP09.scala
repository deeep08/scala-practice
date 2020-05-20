package db.s99

import scala.annotation.tailrec

object PackDuplicatesP09 {
  def main(args: Array[String]): Unit = {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e);
    println(pack(ls))
    // List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

    println(ls span (_ == ls.head))
    println(ls takeWhile(_ == ls.head))
    println(ls dropWhile(_ == ls.head))

    println(packSpan(ls))
    println(packSpan(Nil))

  }

  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def packHelper(ls: List[A], acc: List[A], res: List[List[A]]): List[List[A]] = {
      ls match {
        case Nil => res :+ acc
        case h :: t => if(acc.isEmpty || h == acc.head) packHelper(t, h :: acc, res) else packHelper(t, List(h), res :+ acc)
      }
    }

    packHelper(list, List.empty[A], List.empty[List[A]])
  }

  def packSpan[A](list: List[A]): List[List[A]] = {
    val (packed, rem) = list span {_ == list.head}
    if(rem == Nil) List(packed)
    else packed +: packSpan(rem)
  }
}
