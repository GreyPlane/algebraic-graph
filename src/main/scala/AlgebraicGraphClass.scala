import AlgebraicGraphClass.Graph.Aux

object AlgebraicGraphClass {
  case class Relation[T](domain: Set[T], relation: Set[(T, T)])

  trait Graph[G] {

    type Vertex

    val empty: G

    def vertex(vertex: Vertex): G

    def overlay(x: G, y: G): G

    def connect(x: G, y: G): G

  }

  object Graph {
    type Aux[G, V] = Graph[G] { type Vertex = V }

    def apply[G, V](implicit graph: Graph.Aux[G, V]): Graph.Aux[G, V] = graph

    def instance[G, V](
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



  trait GraphFunctor[A] {
    def gfor[G, V](f: A => V)(implicit g: Graph.Aux[G, V]): G
  }

  implicit def graphFunctorGraph[A]: Graph.Aux[GraphFunctor[A], A] =
    Graph.instance(
      new GraphFunctor[A] {
        def gfor[G, V](f: A => V)(implicit g: Aux[G, V]): G = g.empty
      },
      v =>
        new GraphFunctor[A] {
          def gfor[G, V](f: A => V)(implicit g: Aux[G, V]): G = g.vertex(f(v))
        },
      (x, y) =>
        new GraphFunctor[A] {
          def gfor[G, V](f: A => V)(implicit g: Aux[G, V]): G =
            g.overlay(x.gfor(f)(g), y.gfor(f)(g))
        },
      (x, y) =>
        new GraphFunctor[A] {
          def gfor[G, V](f: A => V)(implicit g: Aux[G, V]): G =
            g.connect(x.gfor(f)(g), y.gfor(f)(g))
        }
    )

  implicit def relationGraph[V]: Graph.Aux[Relation[V], V] = Graph.instance(
    Relation(Set.empty, Set.empty),
    v => Relation(Set(v), Set.empty),
    (x, y) => Relation(x.domain union y.domain, x.relation union y.relation),
    (x, y) =>
      Relation(
        x.domain union y.domain,
        x.relation union y.relation union (for {
          a <- x.domain; b <- y.domain
        } yield (a, b))
      )
  )

  def clique[G, V](xs: List[V])(implicit g: Graph.Aux[G, V]): G =
    xs.map(g.vertex).foldRight(g.empty)(g.connect)

}