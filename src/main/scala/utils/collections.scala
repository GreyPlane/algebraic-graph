package utils

import cats.kernel.Order
import cats.implicits._

import scala.collection.immutable.TreeMap
import scala.collection.mutable

extension [T](x: List[T]) {
  def groupBy_[K](
      f: T => K
  )(implicit order: Order[K]): TreeMap[K, List[T]] = {
    val m = mutable.TreeMap.empty[K, mutable.Builder[T, List[T]]]
    val it = x.iterator
    while (it.hasNext) {
      val elem = it.next()
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, List.newBuilder[T])
      bldr += elem
    }
    var result = TreeMap.empty[K, List[T]]
    val mapIt = m.iterator
    while (mapIt.hasNext) {
      val (k, v) = mapIt.next()
      result = result.updated(k, v.result())
    }
    result
  }

  def partition(step: Int, lag: Int): List[List[T]] = {
    val m = mutable.Map.empty[Int, mutable.Builder[T, List[T]]]
    val totalPartition = x.length / step
    val ixs = x.indices
    for (i <- ixs) {
      val elem = x(i)
      val key = i / step
      val bldr = m.getOrElseUpdate(key, List.newBuilder[T])
      bldr += elem
      if (
        lag != 0 && i != 0 && (i + 1) % totalPartition == 0 && i != x.length - 1
      ) {
        for (j <- Range(i, i + lag)) {
          val extraElem = x(j + 1)
          bldr += extraElem
        }
      }
    }
    var result = List.empty[List[T]]
    val it = m.iterator
    while (it.hasNext) {
      val (_, v) = it.next()
      result = result.appended(v.result())
    }
    result
  }
}

extension [K, V](x: TreeMap[K, V]) {
  def unionWith(y: TreeMap[K, V], f: (V, V) => V)(using
      Order[K]
  ): TreeMap[K, V] = (List.from(x) ++ List.from(y)).groupBy_(_._1).map {
    case (k, kv) => (k, kv.map(_._2).reduce(f))
  }
}
