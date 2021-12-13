import cats.implicits._
import cats.kernel.Order
import shapeless._
import shapeless.newtype._

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable

object Data {

  implicit class EnrichList[T](val x: List[T]) {
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
  }


  implicit class EnrichTreeMap[K, V](val x: TreeMap[K, V]) {
    def unionWith(y: TreeMap[K, V], f: (V, V) => V)(implicit
        orderK: Order[K]
    ): TreeMap[K, V] = (List.from(x) ++ List.from(y)).groupBy_(_._1).map {
      case (k, kv) => (k, kv.map(_._2).reduce(f))
    }
  }
//
//  type Checked[A] = Either[String, A]
//
//  type CheckedT[F[_], A] = Newtype[F[Checked[A]], CheckedTOps[F, A]]
//
//  def CheckedT[F[_], A](v: F[Checked[A]]): CheckedT[F, A] = newtype(v)
//
//  case class CheckedTOps[F[_], A](v: F[Checked[A]]) {
//    val getCheckedT: F[Checked[A]] = v
//  }
//  implicit def mkCheckedTOps[F[_], A](v: F[Checked[A]]): CheckedTOps[F, A] =
//    CheckedTOps[F, A](v)

  private type InnerMap[A] = TreeMap[A, TreeSet[A]]

  type AdjacencyMap[A] = Newtype[InnerMap[A], AdjacencyMapOps[A]]

  object AdjacencyMap {
    def apply[A](v: InnerMap[A]): AdjacencyMap[A] = newtype(v)

    def empty[A](implicit order: Order[A]): AdjacencyMap[A] =
      AdjacencyMap[A](TreeMap.empty[A, TreeSet[A]])

    def vertex[A](x: A)(implicit order: Order[A]): AdjacencyMap[A] =
      AdjacencyMap(TreeMap(x -> TreeSet.empty[A]))

    def overlay[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(implicit
        order: Order[A]
    ): AdjacencyMap[A] = AdjacencyMap(
      x.adjacencyMap.unionWith(y.adjacencyMap, _ union _)
    )

    def connect[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(implicit
        order: Order[A]
    ): AdjacencyMap[A] = AdjacencyMap(
      List(
        x.adjacencyMap,
        y.adjacencyMap,
        TreeMap.from(
          x.adjacencyMap.keys.map(_ -> TreeSet.from(y.adjacencyMap.keys))
        )
      ).reduce((a, b) => a.unionWith(b, _ union _))
    )

    def fromAdjacencySets[A](
        ss: List[(A, Set[A])]
    )(implicit order: Order[A]): AdjacencyMap[A] = {
      val vs =
        TreeMap.from(ss.map(_._2).reduce(_ union _).map(_ -> TreeSet.empty[A]))
      val es = ss.groupBy_(_._1).map { case (k, kvs) =>
        k -> TreeSet.from(kvs.map(_._2).reduce(_ union _))
      }
      AdjacencyMap(vs.unionWith(es, _ union _))
    }

  }
//  def AdjacencyMap[A](v: InnerMap[A]): AdjacencyMap[A] = newtype(v)

//  implicit class AdjacencyMapOps[A](val adjacencyMap: InnerMap[A]) extends AnyVal {
//    def overlay(y: AdjacencyMap[A]) = adjacencyMap.concat(y.adjacencyMap)
//  }

  case class AdjacencyMapOps[A](v: InnerMap[A]) {
    @inline def adjacencyMap: InnerMap[A] = v
  }
  implicit def ops[A](v: InnerMap[A]): AdjacencyMapOps[A] = AdjacencyMapOps(v)

  object implicits {
    implicit class EnrichAny[A](val x: A)(implicit
        ord: Order[A]
    ) {
      val vertex: AdjacencyMap[A] = AdjacencyMap.vertex(x)
      def edge(y: A): AdjacencyMap[A] = AdjacencyMap(
        if (x === y) TreeMap(x -> TreeSet.empty[A])
        else TreeMap(x -> TreeSet(y), y -> TreeSet.empty[A])
      )
    }
    implicit class EnrichAdjacencyMap[A](val m: AdjacencyMap[A])
        extends AnyVal {

      def edgeList(implicit order: Order[A]): List[(A, A)] = for {
        (x, ys) <- m.adjacencyMap.toList
        y <- ys.toList
      } yield x -> y

      def vertexCount(implicit order: Order[A]): Int = m.adjacencyMap.size
      def edgeCount(implicit order: Order[A]): Int =
        m.adjacencyMap.map(_._2.size).sum
      def vertexSet(implicit order: Order[A]): TreeSet[A] =
        m.adjacencyMap.keySet
      def edgeSet(implicit order: Order[A]): TreeSet[(A, A)] =
        TreeSet.from(m.edgeList)

    }
    implicit def adjacencyMapEq[A](implicit
        order: Order[A]
    ): Order[AdjacencyMap[A]] = new Order[AdjacencyMap[A]] {
      def compare(x: AdjacencyMap[A], y: AdjacencyMap[A]): Int = List(
        x.vertexCount compare y.vertexCount,
        x.vertexSet.toSet.tryCompare(y.vertexSet.toSet).get,
        x.edgeCount compare y.edgeCount,
        x.edgeSet.toSet.tryCompare(y.edgeSet).get
      ).combineAll
    }
  }

}
