package fintech.homework07

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class SortingSpec extends FlatSpec with Matchers {

  import Sorting._

  trait A {
    val randomArray: Seq[Double] = 1 to 100 map (_ => Random.nextDouble())

    def isSorted(arr: Seq[Double]): Boolean = arr.init zip arr.tail forall { case (v1, v2) => v1 <= v2 }

    val simple: Seq[Int] = Seq(2, 4, 1, 0, 3).sorted
    val sortedSimple = Seq(0, 1, 2, 3, 4)
  }

  it should "mergeSort is correctly" in new A {
    isSorted(randomArray.mergeSort) shouldBe true
    simple.mergeSort shouldBe sortedSimple
  }

  it should "quickSort is correctly" in new A {
    isSorted(randomArray.quickSort) shouldBe true
    simple.quickSort shouldBe sortedSimple
  }
}
