package db.s99

// P12 (**) Decode a run-length encoded list.
//     Given a run-length code list generated as specified in problem P10,
//     construct its uncompressed version.
//
//     Example:
//     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
object RunLengthDecodingP12 {
  def main(args: Array[String]): Unit = {
    println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
    println(decodeMake(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  }

  def decodeMake[A](list: List[(Int, A)]): List[A] = {
    list flatMap {
      x => List.fill(x._1)(x._2)
    }
  }

  def decode[A](list: List[(Int, A)]): List[A] = {
    list match {
      case head :: tail if head._1 > 0 => head._2 :: decode((head._1 - 1, head._2) :: tail)
      case _ :: tail => decode(tail)
      case Nil => Nil
    }
  }
}
