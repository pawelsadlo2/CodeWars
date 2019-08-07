import org.scalatest._
import org.scalatest.prop._
import org.scalactic.anyvals._
import org.scalatest.funsuite._
import AuxMethods._

class testSuite extends AnyFunSuite with Matchers with GeneratorDrivenPropertyChecks {

  test("from>=to") {
    val nr: Long = 12345
    assert(moveLong(nr)(4, 0) == 51234)
    assert(moveLong(nr)(2, 0) == 31245)

    assert {
      val replaced: List[Long] = (for {
        x <- nr.toString.indices
      } yield moveLong(nr)(x, x)).toList

      replaced.indexWhere(x => x != nr) == -1
    }
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assert(moveLong(12345)(0, 4) == 23451)
    assert(moveLong(12345)(0, 3) == 23415)
  }

  def sameIndex(nr: Long) = {
    forAll(posZIntsBetween(0, PosZInt.fromOrElse(nr.toString.length - 1, 0))) { i: PosZInt =>
      assert(nr == moveLong(nr)(i, i))
    }
  }


  /*  def sameIndex(nr: Long) = {
      forAll(posZIntsBetween(0, PosZInt.fromOrElse(nr.toString.length - 1, 0))) { i: PosZInt =>
        assert(moveLong(nr)(i, i) == moveLong(nr)(i, i))
      }
    }*/

  def sameDigitNumber(digit: Int, times: Long): Long = (1L to times).map(_ => digit).mkString("").toLong


  def forAllNumbersOfLengthBetween(from: Long, to: Long)(f: Long => Assertion) = {
    val fromPozZLong = PosZLong.fromOrElse(from, 0L)
    val toPozZLong = PosZLong.fromOrElse(to, 0L)
    forAll(posZLongsBetween(fromPozZLong, toPozZLong)) { nr: PosZLong =>
      f(nr.toLong)
    }
  }


  test("replacing number with same index") {
    forAllNumbersOfLengthBetween(0L, Long.MaxValue)(sameIndex)
  }

  test("toSmallestTests") {
    import ToSmallestTest.testing
    testing(261235, Array(126235, 2, 0))
    testing(256687587015L, Array(25668758715L, 9, 0))
    testing(935855753L, Array(358557539, 0, 8))
    testing(285365,  Array(238565, 3, 1))
  }

}


object ToSmallestTest {
   def testing(n: Long, exp: Array[Long]): Unit = {
    val act = ToSmallest.smallest(n)
    val actual: String = act.mkString(", ")
    val expect: String = exp.mkString(", ")
    println("Testing " + n)
    println("Actual --> " + actual)
    println("Expect --> " + expect)
    println("-")
    assert(expect==actual)
  }
}



