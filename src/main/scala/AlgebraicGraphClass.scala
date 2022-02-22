import Relation._
import cats.Order
import cats.Eq
import cats.implicits._
import scala.collection.immutable.TreeSet

object AlgebraicGraphClass {
  trait Graph[G] {

    type Vertex

    val empty: G

    def vertex(vertex: Vertex): G

    def overlay(x: G, y: G): G

    def connect(x: G, y: G): G

  }

  object Graph {
    type Aux[G, V] = Graph[G] { type Vertex = V }

    def apply[G, V](
        e: G,
        f1: V => G,
        f2: (G, G) => G,
        f3: (G, G) => G
    ): Aux[G, V] = new Graph[G] {
      type Vertex = V
      val empty: G = e

      def vertex(vertex: V): G = f1(vertex)

      def overlay(x: G, y: G): G = f2(x, y)

      def connect(x: G, y: G): G = f3(x, y)
    }
  }

  opaque type GraphFunctor[A] = [G, V] => (f: A => V) => Graph.Aux[G, V] ?=> G
  object GraphFunctor {
    def apply[A] = identity[GraphFunctor[A]]
  }

  extension [A](x: GraphFunctor[A])
    def gmap[G, V](f: A => V)(using Graph.Aux[G, V]): G = x(f)

  given [A]: Graph.Aux[GraphFunctor[A], A] = Graph(
    [G, V] => (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.empty,
    v => [G, V] => (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.vertex(f(v)),
    (x: GraphFunctor[A], y: GraphFunctor[A]) =>
      [G, V] =>
        (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.overlay(x.gmap(f), y.gmap(f)),
    (x: GraphFunctor[A], y: GraphFunctor[A]) =>
      [G, V] =>
        (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.connect(x.gmap(f), y.gmap(f)),
  )

  opaque type GraphMonad[A] = [G, V] => (f: A => G) => Graph.Aux[G, V] ?=> G
  object GraphMonad {
    def apply[A] = identity[GraphMonad[A]]
  }

  extension [A](x: GraphMonad[A])
    def bind[G, V](f: A => G)(using Graph.Aux[G, V]): G = x(f)

  given [A]: Graph.Aux[GraphMonad[A], A] = Graph(
    [G, V] => (f: A => G) => (g: Graph.Aux[G, V]) ?=> g.empty,
    v => [G, V] => (f: A => G) => (g: Graph.Aux[G, V]) ?=> f(v),
    (x: GraphMonad[A], y: GraphMonad[A]) =>
      [G, V] =>
        (f: A => G) => (g: Graph.Aux[G, V]) ?=> g.overlay(x.bind(f), y.bind(f)),
    (x: GraphMonad[A], y: GraphMonad[A]) =>
      [G, V] =>
        (f: A => G) => (g: Graph.Aux[G, V]) ?=> g.connect(x.bind(f), y.bind(f))
  )

  given relationGraph[V](using Order[V]): Graph.Aux[Relation[V], V] =
    Graph(
      Relation(TreeSet.empty, TreeSet.empty),
      v => Relation(TreeSet(v), TreeSet.empty),
      (x, y) => Relation(x.domain union y.domain, x.relation union y.relation),
      (x, y) =>
        Relation(
          x.domain union y.domain,
          x.relation union y.relation union (for {
            a <- x.domain; b <- y.domain
          } yield (a, b))
        )
    )

  object extensions {
    extension [A](x: Relation[A])(using g: Graph.Aux[Relation[A], A]) {
      def `+`(y: Relation[A]): Relation[A] = g.overlay(x, y)

      def `->`(y: Relation[A]): Relation[A] = g.connect(x, y)
    }
  }

  def vertices[G, V](xs: List[V])(using g: Graph.Aux[G, V]): G =
    xs.map(g.vertex).foldRight(g.empty)(g.overlay)

  def edge[G, V](x: V, y: V)(using g: Graph.Aux[G, V]): G =
    g.connect(g.vertex(x), g.vertex(y))

  def clique[G, V](vs: List[V])(using g: Graph.Aux[G, V]): G =
    vs.map(g.vertex).foldRight(g.empty)(g.connect)

  def isSubgraphOf[G, V](x: G, y: G)(using
      g: Graph.Aux[G, V],
      eq: Eq[G]
  ): Boolean = g.overlay(x, y) === y

  def edges[G, V](es: List[(V, V)])(using g: Graph.Aux[G, V]): G =
    es.map { case (x, y) => edge(x, y) }.foldRight(g.empty)(g.overlay)

  def graph[G, V](vs: List[V], es: List[(V, V)])(using g: Graph.Aux[G, V]): G =
    g.overlay(vertices(vs), edges(es))

  def path[G, V](vs: List[V])(using g: Graph.Aux[G, V]): G = edges(
    vs.zip(vs.tail)
  )

  def star[G, V](v: V, vs: List[V])(using g: Graph.Aux[G, V]): G =
    g.connect(g.vertex(v), vertices(vs))

  def induce[G, V](p: V => Boolean, gm: GraphMonad[V])(using
      g: Graph.Aux[G, V]
  ): G = gm.bind[G, V](x => if (p(x)) g.vertex(x) else g.empty)

}
