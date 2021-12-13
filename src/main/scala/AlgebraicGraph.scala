

object AlgebraicGraph {
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
    type Graph
    type Vertex
    def gfor(f: A => Vertex)(implicit g: Graph.Aux[Graph, A]): Graph
  }

  object GraphFunctor {
    type Aux[A, G, V] = GraphFunctor[A] { type Graph = G; type Vertex = V }

    def apply[A, G, V](ff: (A => V) => G): Aux[A, G, V] =
      new GraphFunctor[A] {
        type Graph = G
        type Vertex = V
        def gfor(f: A => V)(implicit g: Graph.Aux[G, A]): G = ff(f)
      }
  }

  //  implicit def graphFunctorGraph[A, G, V](implicit g: Graph.Aux[G, V]): Graph.Aux[GraphFunctor.Aux[A, G, V], A] = Graph.instance(
  //    GraphFunctor(_ => g.empty),
  //    x => GraphFunctor(f => g.vertex(f(x))),
  //    ???, ???
  //  )
  //
  //  implicit def graphFunctor[A, G, V](implicit g: Graph.Aux[G, V]): GraphFunctor.Aux[A, G, V] = ???

  implicit def relationGraph[V]: Graph.Aux[Relation[V], V] = Graph.instance(
    Relation(Set.empty, Set.empty),
    (v) => Relation(Set(v), Set.empty),
    (x, y) => Relation(x.domain union y.domain, x.relation union y.relation),
    (x, y) =>
      Relation(
        x.domain union y.domain,
        x.relation union y.relation union (for {
          a <- x.domain; b <- y.domain
        } yield (a, b))
      )
  )

  // implicit def graphFunctorGraph[V]: Graph.Aux[GraphFunctor[V], V]

  def clique[G, V](xs: List[V])(implicit g: Graph.Aux[G, V]): G =
    xs.map(g.vertex).foldRight(g.empty)(g.connect)

  //  def gmap[A, G, V](f: A => V, gf: GraphFunctor.Aux[A, G, V])(implicit g: Graph.Aux[G, V]): G = gf.gfor(f)

  val g: Relation[Int] = clique(List(1, 2, 3))

  //  val gg: GraphFunctor.Aux[Int, Relation[Int], String] = clique(List(1,2,3))
  //
  //  val fg = gg.gfor(x => x.toString)
  //  val fg = gmap[Int, Relation[String], String](_.toString, gg)
  val x = 1
}
