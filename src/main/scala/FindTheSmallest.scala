import scala.reflect.ClassTag

object ToSmallest {

  import AuxMethods._

  def smallest(n: Long): Array[Long] = {
    val nArray = longToArray(n)
    val indices = nArray.indices

    val candidates = for {
      fromI <- indices
      toI <- indices
      if fromI != toI
    } yield Array(arrayToLong(move(nArray)(fromI, toI)), fromI, toI)

    candidates.minBy(_.head)
  }
}

object AuxMethods {
  def longToArray(n: Long): Array[Int] = n.toString.map((x: Char) => x.asDigit).toArray

  def arrayToLong[T: ClassTag](a: Array[T]) = a.mkString("").toLong

  def move[T: ClassTag](array: Array[T])(fromI: Int, toI: Int): Array[T] = {
    val temp = array(fromI)
    val (beforeFromI, afterFromI) = array.splitAt(fromI)
    if (toI <= fromI) {
      val (beforeToI, afterToI) = beforeFromI.splitAt(toI)

      beforeToI :+ temp :++ afterToI :++ afterFromI.tail
    } else {
      val (beforeToI, afterToI) = afterFromI.splitAt(toI - fromI + 1)
      beforeFromI :++ beforeToI.tail :+ temp :++ afterToI
    }
  }

  def moveLong(n: Long): (Int, Int) => Long = move(longToArray(n))(_, _).mkString("").toLong

  def commonPartTakeIndex[T](arr1: Array[T], arr2: Array[T]) =
    arr1.indices.indexWhere(idx => !(arr1(idx) == arr2(idx)))

}

