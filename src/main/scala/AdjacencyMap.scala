import cats.implicits.*
import cats.kernel.Order

import scala.collection.immutable.{TreeMap, TreeSet}
import utils.*
import cats.Functor

object AdjacencyMap {

  opaque type AdjacencyMap[A] = TreeMap[A, TreeSet[A]]

  def apply[A]: AdjacencyMap[A] => AdjacencyMap[A] = identity

  extension [A](m: AdjacencyMap[A])(using Order[A]) {

    def unwarp: TreeMap[A, TreeSet[A]] = m

    def edgeList: List[(A, A)] = for {
      (x, ys) <- m.toList
      y <- ys.toList
    } yield x -> y

    def vertexCount: Int = m.size
    def edgeCount: Int = m.map(_._2.size).sum
    def vertexSet: TreeSet[A] =
      m.keySet
    def edgeSet: TreeSet[(A, A)] =
      TreeSet.from(m.edgeList)

  }

  def empty[A](implicit order: Order[A]): AdjacencyMap[A] =
    AdjacencyMap[A](TreeMap.empty[A, TreeSet[A]])

  def vertex[A](x: A)(implicit order: Order[A]): AdjacencyMap[A] =
    AdjacencyMap(TreeMap(x -> TreeSet.empty[A]))

  def overlay[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(using
      Order[A]
  ): AdjacencyMap[A] = AdjacencyMap(
    x.unionWith(y, _ union _)
  )

  def connect[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(using
      Order[A]
  ): AdjacencyMap[A] = AdjacencyMap(
    List(
      x,
      y,
      TreeMap.from(
        x.keys.map(_ -> TreeSet.from(y.keys))
      )
    ).reduce((a, b) => a.unionWith(b, _ union _))
  )

  def fromAdjacencySets[A](
      ss: List[(A, Set[A])]
  )(implicit order: Order[A]): AdjacencyMap[A] = {
    val vs =
      TreeMap.from(ss.map(_._2).reduce(_ union _).map(_ -> TreeSet.empty[A]))
    val es = ss.groupBy_(_._1).map { case (k, kvs) =>
      k -> TreeSet.from(kvs.map(_._2).reduce[Set[A]](_ union _))
    }
    AdjacencyMap(vs.unionWith(es, _ union _))
  }

  object extensions {
    extension [A](x: A)(using ord: Order[A]) {
      def vertex: AdjacencyMap[A] = AdjacencyMap.vertex(x)
      def edge(y: A): AdjacencyMap[A] = AdjacencyMap(
        if (x === y) TreeMap(x -> TreeSet.empty[A])
        else TreeMap(x -> TreeSet(y), y -> TreeSet.empty[A])
      )
    }

  }

  given adjacencyMapOrder[A](using Order[A]): Order[AdjacencyMap[A]] =
    import extensions._
    (x: AdjacencyMap[A], y: AdjacencyMap[A]) => List(
      x.vertexCount compare y.vertexCount,
      x.vertexSet.toSet.tryCompare(y.vertexSet.toSet).get,
      x.edgeCount compare y.edgeCount,
      x.edgeSet.toSet.tryCompare(y.edgeSet).get
    ).combineAll

}
