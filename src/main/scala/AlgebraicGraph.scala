import Data.AdjacencyMap
import cats.Monad
import cats.Show
import cats.kernel.Order
import cats.implicits._

import scala.collection.immutable.TreeSet

object AlgebraicGraph {

  sealed trait Graph[+A]
  case object Empty extends Graph[Nothing]
  final case class Vertex[A](a: A) extends Graph[A]
  final case class Overlay[A](x: Graph[A], y: Graph[A]) extends Graph[A]
  final case class Connect[A](x: Graph[A], y: Graph[A]) extends Graph[A]

  def foldg[A, B](e: B, v: A => B, o: (B, B) => B, c: (B, B) => B)(
      g: Graph[A]
  ): B =
    g match {
      case Empty         => e
      case Vertex(x)     => v(x)
      case Overlay(x, y) => o(foldg(e, v, o, c)(x), foldg(e, v, o, c)(y))
      case Connect(x, y) => c(foldg(e, v, o, c)(x), foldg(e, v, o, c)(y))
    }

  object syntax {
    implicit class GraphOps[A](val g: Graph[A]) extends AnyVal {
      def fold[B](e: B, v: A => B, o: (B, B) => B, c: (B, B) => B): B =
        foldg(e, v, o, c)(g)

      def `+`(y: Graph[A]): Graph[A] = Overlay(g, y)

      def `*`(y: Graph[A]): Graph[A] = Connect(g, y)

      import Data.implicits._

      def toAdjacencyMap(implicit order: Order[A]): AdjacencyMap[A] =
        g.fold[AdjacencyMap[A]](
          AdjacencyMap.empty[A],
          _.vertex,
          AdjacencyMap.overlay,
          AdjacencyMap.connect
        )

      def vertexSet(implicit order: Order[A]): TreeSet[A] = g.fold[TreeSet[A]](
        TreeSet.empty[A],
        TreeSet(_),
        _ union _,
        _ union _
      )

      def edgeList(implicit order: Order[A]) = g.toAdjacencyMap.edgeList

      def toList: List[A] = g match {
        case Empty         => List.empty[A]
        case Vertex(a)     => List(a)
        case Overlay(x, y) => x.toList ++ x.toList
        case Connect(x, y) => x.toList ++ y.toList
      }

      def mergeVertices(p: A => Boolean, v: A)(implicit order: Order[A]) =
        Graph.mergeVertices(p, v, g)
    }
  }

  object Graph {
    import syntax._
    import implicits._
    import collectionExts._

    def empty[A]: Graph[A] = Empty
    @inline def vertex[A](a: A): Graph[A] = Vertex(a)
    @inline def overlay[A](x: Graph[A], y: Graph[A]): Graph[A] = Overlay(x, y)
    @inline def connect[A](x: Graph[A], y: Graph[A]): Graph[A] = Connect(x, y)
    def overlays[A](xs: List[Graph[A]]) = xs.foldRight(empty[A])(_ + _)
    def connects[A](xs: List[Graph[A]]) = xs.foldRight(empty[A])(_ * _)
    def edge[A](e: (A, A)): Graph[A] = vertex(e._1) * vertex(e._2)
    def edges[A](es: List[(A, A)]) = overlays(es.map(edge))
    def vertices[A](vs: List[A]) = overlays(vs.map(vertex))
    def clique[A](vs: List[A]) = connects(vs.map(vertex))
    def star[A](v: A, vs: List[A]): Graph[A] = connect(vertex(v), vertices(vs))
    def ladder[A](vs: List[A], step: Int): Graph[A] = overlays(vs.partition(step, 1).map(vvs => star(vvs.head, vvs.tail)))
    def path[A](vs: List[A]): Graph[A] = vs match {
      case Nil => empty[A]
      case x :: Nil => vertex(x)
      case _ => overlays(vs.zip(vs.tail).map(v => connect(vertex(v._1), vertex(v._2))))
    }
    def mergeVertices[A](p: A => Boolean, v: A, g: Graph[A])(implicit
        order: Order[A]
    ) = g.map(u => if (p(u)) v else u)
    def box[A, B](x: Graph[A], y: Graph[B]): Graph[(A, B)] = {
      val xs = y.toList.map(b => x.map(_ -> b))
      val ys = x.toList.map(a => y.map(a -> _))
      overlays(xs ++ ys)
    }
  }

  object implicits {

    import syntax._

    //    implicit def graphApplicative: Applicative[Graph] = new Applicative[Graph] {
    //      def pure[A](x: A): Graph[A] = Vertex(x)
    //
    //      def ap[A, B](ff: Graph[A => B])(fa: Graph[A]): Graph[B] = ff.foldg(
    //        Empty,
    //        f => fa.foldg(???, f, ???, ???),
    //        Overlay[B],
    //        Connect[B]
    //      )
    //    }

    implicit def graphMonad: Monad[Graph] = new Monad[Graph] {
      def flatMap[A, B](fa: Graph[A])(f: A => Graph[B]): Graph[B] =
        fa.fold(Graph.empty[B], f, Overlay[B], Connect[B])

      def tailRecM[A, B](a: A)(f: A => Graph[Either[A, B]]): Graph[B] = f(
        a
      ) match {
        case Empty     => Empty
        case Vertex(a) => a.fold(tailRecM(_)(f), Vertex(_))
        case Overlay(x, y) =>
          x.fold(
            Empty,
            _.fold(tailRecM(_)(f), Vertex(_)),
            Overlay[B],
            Connect[B]
          ) + y.fold(
            Empty,
            _.fold(tailRecM(_)(f), Vertex(_)),
            Overlay[B],
            Connect[B]
          )
        case Connect(x, y) =>
          x.fold(
            Empty,
            _.fold(tailRecM(_)(f), Vertex(_)),
            Overlay[B],
            Connect[B]
          ) * y.fold(
            Empty,
            _.fold(tailRecM(_)(f), Vertex(_)),
            Overlay[B],
            Connect[B]
          )
      }

      def pure[A](x: A): Graph[A] = Vertex(x)
    }

    implicit def graphShow[A]: Show[Graph[A]] = new Show[Graph[A]] {
      def show(t: Graph[A]): String = {
        def go(p: Boolean, graph: Graph[A]): String = graph match {
          case Empty              => "empty"
          case Vertex(x)          => x.toString
          case Overlay(x, y) if p => "(" + go(false, graph) + ")"
          case Overlay(x, y)      => go(false, x) + " + " + go(false, y)
          case Connect(x, y)      => go(true, x) + " * " + go(true, y)
        }

        go(false, t);
      }
    }

  }
}
