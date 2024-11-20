package ag

import utils.*
import cats.data.NonEmptyList
import cats.implicits.*
import cats.kernel.{Order, PartialOrder}

import scala.annotation.tailrec
import scala.collection.immutable.{TreeMap, TreeSet}
import scala.util.control.ControlThrowable

object AdjacencyMaps {
  opaque type AdjacencyMap[A] = TreeMap[A, TreeSet[A]]

  object AdjacencyMap {
    @inline def apply[A](m: AdjacencyMap[A]): AdjacencyMap[A] = m
  }

  extension [A](m: AdjacencyMap[A])(using Order[A]) {

    def underlying: TreeMap[A, TreeSet[A]] = m

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

    def paths(root: A): List[List[A]] =
      m.getOrElse(root, TreeSet.empty[A]).toList match {
        case Nil => List(List(root))
        case es  => es.flatMap(m.paths(_).map(root :: _))
      }

    def postSet(x: A): TreeSet[A] = {
      m.getOrElse(x, TreeSet.empty)
    }

    def topoSort = {
      case class SortState[A](
          parent: collection.mutable.TreeMap[A, A],
          entry: collection.mutable.TreeMap[A, NodeState],
          order: collection.mutable.ListBuffer[A]
      )
      enum NodeState {
        case Exited
        case Entered
      }
      type Cycle[A] = NonEmptyList[A]

      case class BreakCycle[A](x: Cycle[A]) extends ControlThrowable

      import NodeState.*
      val nodeState = SortState[A](
        collection.mutable.TreeMap.empty,
        collection.mutable.TreeMap.empty,
        collection.mutable.ListBuffer.empty
      )
      val vertices = m.keySet.toList.reverse

      def adjacent(x: A) = m.postSet(x).toList.reverse

      def exit(v: A) = {
        nodeState.entry.updateWith(v)(_.map {
          case Entered =>
            Exited
          case Exited =>
            throw new IllegalArgumentException("wrong state")
        })
        nodeState.order.prepend(v)
      }

      def retrace(
          curr: A,
          head: A,
          parent: collection.mutable.TreeMap[A, A]
      ) = {
        @tailrec
        def aux(xs: NonEmptyList[A]): NonEmptyList[A] = {
          if (xs.head == head) xs
          else aux(parent.get(xs.head).map(c => xs.prepend(c)).get)
        }

        aux(NonEmptyList.of(curr))
      }

      def dfs(x: A): Unit = adjacent(x).foreach(y =>
        nodeState.entry.get(y) match {
          case None => {
            nodeState.parent.update(y, x)
            nodeState.entry.update(y, Entered)
            dfs(y)
            exit(y)
          }
          case Some(Exited) => ()
          case Some(Entered) => {
            val e = BreakCycle(retrace(x, y, nodeState.parent))
            throw e
          }
        }
      )

      def dfsRoot(x: A): Unit = nodeState.entry.get(x) match {
        case None => {
          nodeState.entry.update(x, Entered)
          dfs(x)
          exit(x)
        }
        case _ => ()
      }

      try {
        vertices.foreach(dfsRoot)
        Right(nodeState.order.toList)
      } catch {
        case e: BreakCycle[_] =>
          Left(e.x)
      }
    }

  }

  def empty[A](implicit order: Order[A]): AdjacencyMap[A] =
    AdjacencyMap[A](TreeMap.empty[A, TreeSet[A]])

  def vertex[A](x: A)(implicit order: Order[A]): AdjacencyMap[A] =
    AdjacencyMap[A](TreeMap(x -> TreeSet.empty[A]))

  def overlay[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(using Order[A]): AdjacencyMap[A] =
    AdjacencyMap[A](x.unionWith(y, _ union _))

  def connect[A](x: AdjacencyMap[A], y: AdjacencyMap[A])(using Order[A]): AdjacencyMap[A] =
    AdjacencyMap[A](
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
      def vertex: AdjacencyMap[A] = AdjacencyMaps.vertex(x)
      def edge(y: A): AdjacencyMap[A] = AdjacencyMap[A](
        if (x === y) TreeMap(x -> TreeSet.empty[A])
        else TreeMap(x -> TreeSet(y), y -> TreeSet.empty[A])
      )
    }
  }

  given adjacencyMapOrder[A](using Order[A]): PartialOrder[AdjacencyMap[A]] =
    (x: AdjacencyMap[A], y: AdjacencyMap[A]) =>
      List(
        (x.vertexCount compare y.vertexCount).toDouble,
        x.vertexSet.toSet
          .tryCompare(y.vertexSet.toSet)
          .map(_.toDouble)
          .getOrElse(Double.NaN),
        (x.edgeCount compare y.edgeCount).toDouble,
        x.edgeSet.toSet
          .tryCompare(y.edgeSet.toSet)
          .map(_.toDouble)
          .getOrElse(Double.NaN)
      ).combineAll

}
