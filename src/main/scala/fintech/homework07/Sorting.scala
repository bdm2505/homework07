package fintech.homework07

import scala.collection.mutable
import scala.math.Ordering


/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting extends App {
  type Arr[T] = mutable.IndexedSeq[T]

  implicit class SortingOpsForMutable[T](val seq: Arr[T]) {
    def mergeSort(implicit ord: Ordering[T]): Sorting.Arr[T] = Sorting.mergeSort(seq)(ord)

    def quickSort(implicit ord: Ordering[T]): Sorting.Arr[T] = Sorting.quickSort(seq)(ord)
  }

  implicit class SortingOpsForImmutable[T](val seq: Seq[T]) {
    def mergeSort(implicit ord: Ordering[T]): Seq[T] = Sorting.mergeSort(new mutable.ArrayBuffer[T] ++= seq)(ord)

    def quickSort(implicit ord: Ordering[T]): Seq[T] = Sorting.quickSort(new mutable.ArrayBuffer[T] ++= seq)(ord)
  }


  def mergeSort[T](seq: Arr[T])(implicit ord: Ordering[T]): Arr[T] = {
    if (seq.size > 1) {
      val c = seq.size / 2
      val left = mergeSort(seq.take(c))
      val right = mergeSort(seq.drop(c))
      var l, r, index = 0
      while (l < left.size && r < right.size) {
        if (ord.compare(left(l), right(r)) < 0) {
          seq(index) = left(l)
          l += 1
        } else {
          seq(index) = right(r)
          r += 1
        }
        index += 1
      }
      while (l < left.size) {
        seq(index) = left(l)
        l += 1
        index += 1
      }
      while (r < right.size) {
        seq(index) = right(r)
        r += 1
        index += 1
      }
      seq
    }
    else
      seq
  }


  def quickSort[K](seq: Arr[K])(implicit ord: Ordering[K]): Arr[K] = {

    def quick(left: Int, right: Int): Unit = {
      if (right - left > 1) {
        var center = (left + right) / 2
        val p = seq(center)
        var iRight = center + 1
        var iLeft = left
        var r = right
        while (center - iLeft > 0) {
          val current = seq(iLeft)
          ord.compare(current, p) match {
            case 0 =>
              seq(iLeft) = seq(center - 1)
              seq(center - 1) = current
              center -= 1
            case x if x < 0 =>
              iLeft += 1
            case _ if r > iRight =>
              seq(iLeft) = seq(r - 1)
              seq(r - 1) = current
              r -= 1
            case _ =>
              seq(iLeft) = seq(center - 1)
              seq(center - 1) = seq(iRight - 1)
              seq(iRight - 1) = current
              center -= 1
              iRight -= 1
              r -= 1
          }
        }
        while (r - iRight > 0) {
          val current = seq(r - 1)
          ord.compare(current, p) match {
            case 0 =>
              seq(r - 1) = seq(iRight)
              seq(iRight) = current
              iRight += 1
            case x if x > 0 =>
              r -= 1
            case _ =>
              seq(r - 1) = seq(iRight)
              seq(iRight) = seq(center)
              seq(center) = current
              iLeft += 1
              center += 1
              iRight += 1
          }
        }
        if (iLeft - left < right - r) {
          quick(left, iLeft)
          quick(r, right)
        }
        else {
          quick(r, right)
          quick(left, iLeft)
        }
      }
    }

    quick(0, seq.length)
    seq
  }

}
