object dotty {
//  @main def hello: Unit = {
//    println(msg)
//
//    def msg = "I was compiled by Scala 3. :)"
//
//    import algebricGraph._
//
//    val g: Relation[Int] = clique(List(1, 2, 3))
//    val gf: GraphFunctor[Int] = clique(List(1, 2, 3))
//    val gggg = gf.gfor[Relation[String], String](_.toString + "a")
//
//  }
//
//  object algebricGraph {
//    case class Relation[T](domain: Set[T], relation: Set[(T, T)])
//
//    trait Graph[G] {
//
//      type Vertex
//
//      val empty: G
//
//      def vertex(vertex: Vertex): G
//
//      def overlay(x: G, y: G): G
//
//      def connect(x: G, y: G): G
//
//    }
//
//    object Graph {
//      type Aux[G, V] = Graph[G] { type Vertex = V }
//
//      def apply[G, V](
//                       e: G,
//                       f1: V => G,
//                       f2: (G, G) => G,
//                       f3: (G, G) => G
//                     ): Aux[G, V] = new Graph[G] {
//        type Vertex = V
//        val empty = e
//
//        def vertex(vertex: V): G = f1(vertex)
//
//        def overlay(x: G, y: G): G = f2(x, y)
//
//        def connect(x: G, y: G): G = f3(x, y)
//      }
//    }
//
//    opaque type GraphFunctor[A] = [G, V] => (f: A => V) => Graph.Aux[G, V] ?=> G
//
//    // val fff: GraphFunctor[Int] = [Relation[String], String] =>
//    //   (f: Int => String) => (g: Graph.Aux[Relation[String], String]) ?=> ???
//
//    object GraphFunctor {
//      def apply[A](f: GraphFunctor[A]): GraphFunctor[A] = f
//    }
//
//    extension [A](x: GraphFunctor[A]) {
//      def gfor[G, V](f: A => V)(using Graph.Aux[G, V]): G = x(f)
//    }
//
//    given [A]: Graph.Aux[GraphFunctor[A], A] = Graph.apply(
//      [G, V] => (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.empty,
//    v => [G, V] => (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.vertex(f(v)),
//    (x: GraphFunctor[A], y: GraphFunctor[A]) =>
//    [G, V] =>
//    (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.overlay(x.gfor(f), y.gfor(f)),
//    (x: GraphFunctor[A], y: GraphFunctor[A]) =>
//    [G, V] =>
//    (f: A => V) => (g: Graph.Aux[G, V]) ?=> g.connect(x.gfor(f), y.gfor(f)),
//    )
//
//    given relationGraph[V]: Graph.Aux[Relation[V], V] = Graph.apply(
//      Relation(Set.empty, Set.empty),
//      (v) => Relation(Set(v), Set.empty),
//      (x, y) => Relation(x.domain union y.domain, x.relation union y.relation),
//      (x, y) =>
//        Relation(
//          x.domain union y.domain,
//          x.relation union y.relation union (for {
//            a <- x.domain; b <- y.domain
//          } yield ((a, b)))
//        )
//    )
//
//    val r = Relation(Set("a", "b", "c"), Set.empty)
//
//    def clique[G, V](xs: List[V])(using g: Graph.Aux[G, V]): G =
//      xs.map(g.vertex).foldRight(g.empty)(g.connect)
//
//  }

}
