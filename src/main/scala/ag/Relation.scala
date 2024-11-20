package ag

import ag.AlgebraicGraph.{Graph as G, *}
import cats.kernel.Eq

import scala.collection.immutable.TreeSet

case class Relation[T](domain: TreeSet[T], relation: TreeSet[(T, T)])
object Relation {

  given relationEq[T](using eq: Eq[T]): Eq[Relation[T]] =
    new Eq[Relation[T]] {
      override def eqv(x: Relation[T], y: Relation[T]): Boolean =
        x.domain.equals(y.domain) && x.relation.equals(y.relation)
    }

  given relationToGraph[T]: ToGraph.Aux[Relation[T], T] =
    new ToGraph[Relation[T]] {
      type Vertex = T

      def toGraph(t: Relation[Vertex]): G[Vertex] =
        G.vertices(t.domain.toList) + G.edges(t.relation.toList)

    }

}
