import cats.implicits._
import cats.kernel.Eq

import scala.collection.immutable.TreeSet

object algGraph {

  case class Relation[T](domain: TreeSet[T], relation: TreeSet[(T, T)])

  implicit def relationEq[T](implicit eq: Eq[T]): Eq[Relation[T]] = new Eq[Relation[T]] {
    override def eqv(x: Relation[T], y: Relation[T]): Boolean = x.domain.equals(y.domain) && x.relation.equals(y.relation)
  }

  trait Graph[G, Vertex] {

    val empty: G

    def vertex(vertex: Vertex): G

    def overlay(x: G, y: G): G

    def connect(x: G, y: G): G
  }

  object implicits {
    implicit def relationGraph[T](implicit ordering: Ordering[T]): Graph[Relation[T], T] = new Graph[Relation[T], T] {

      val empty: Relation[T] = Relation(TreeSet.empty, TreeSet.empty)

      def vertex(vertex: T): Relation[T] = Relation(TreeSet(vertex), TreeSet.empty)

      def overlay(x: Relation[T], y: Relation[T]): Relation[T] = Relation(x.domain union y.domain, x.relation union y.relation)

      def connect(x: Relation[T], y: Relation[T]): Relation[T] = Relation(x.domain union y.domain, x.relation union y.relation union (for { a <- x.domain; b <- y.domain } yield (a, b)))

    }
  }

  object syntax {
    implicit class relationSyntax[T](val relation: Relation[T])(implicit graph: Graph[Relation[T], T]) {
      def `+`(other: Relation[T]): Relation[T] = graph.overlay(relation, other)

      def `->`(other: Relation[T]): Relation[T] = graph.connect(relation, other)
    }
  }

  def vertices[G, V](xs: List[V])(implicit g: Graph[G, V]): G = xs.map(g.vertex).foldRight(g.empty)(g.overlay)

  def edge[G, V](x: V, y: V)(implicit g: Graph[G, V]): G = g.connect(g.vertex(x), g.vertex(y))

  def clique[G, V](vs: List[V])(implicit g: Graph[G, V]): G = vs.map(g.vertex).foldRight(g.empty)(g.connect)

  def isSubgraphOf[G, V](x: G, y: G)(implicit eq: Eq[G], g: Graph[G, V]): Boolean = g.overlay(x, y) === y

  def edges[G, V](es: List[(V, V)])(implicit g: Graph[G, V]): G = es.map { case (x, y) => edge(x, y) }.foldRight(g.empty)(g.overlay)

  def graph[G, V](vs: List[V], es: List[(V, V)])(implicit g: Graph[G, V]): G = g.overlay(vertices(vs), edges(es))

  def path[G, V](vs: List[V])(implicit g: Graph[G, V]): G = edges(vs.zip(vs.tail))

  def star[G, V](v: V, vs: List[V])(implicit g: Graph[G, V]): G = g.connect(g.vertex(v), vertices(vs))

  def scalator[G, V](vss: List[List[V]])(implicit g: Graph[G, V]): G = vss.map(vs => star(vs.head, vs.tail)).foldRight(g.empty)(g.overlay)

}

object model {
  sealed trait BuildBlock
}