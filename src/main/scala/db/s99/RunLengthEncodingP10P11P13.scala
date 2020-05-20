package db.s99

import scala.annotation.tailrec

object RunLengthEncodingP10P11P13 {
  def main(args: Array[String]): Unit = {
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encode(List(1)))

    println(encodeUsingPack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encodeUsingPack(List(1)))

    println(modEncode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(modEncode(List(1)))

    println(encodeSpan(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encodeSpan(List(1)))
  }

  def modEncode[A](list: List[A]) : List[Any] = {
    val tmp = PackDuplicatesP09.packSpan(list)
    tmp map {
      x => if(x.length == 1) x.head else (x.length, x.head)
    }
  }

  def encodeUsingPack[A](list: List[A]): List[(Int, A)] = {
    val temp = PackDuplicatesP09.packSpan(list)
    temp map {
      x => (x.length, x.head)
    }
  }

  def encodeSpan[A](list: List[A]): List[(Int, A)] = {
    val (packed, rest) = list span (_ == list.head)
    if(rest == Nil) List((packed.length, packed.head))
    else (packed.length, packed.head) :: encodeSpan(rest)
  }

  def encode[A](list: List[A]): List[(Int, A)] = {
    @tailrec
    def encodeHelper(list: List[A], count: Int, res: List[(Int, A)]): List[(Int, A)] = {
      list match {
        case Nil => List.empty[(Int, A)]
        case head :: Nil => res :+ (count+1, head)
        case head :: tail => if (head == tail.head) encodeHelper(tail, count+1, res)
                             else encodeHelper(tail, 0, res :+ (count+1, head))
      }
    }

    encodeHelper(list, 0, Nil)
  }
}
