import cats.kernel.Eq

import scala.collection.immutable.TreeSet
import AlgGraphHK.{Graph => G}
import AlgGraphHK.syntax._

object Relation {

  case class Relation[T](domain: TreeSet[T], relation: TreeSet[(T, T)])

  implicit def relationEq[T](implicit eq: Eq[T]): Eq[Relation[T]] =
    new Eq[Relation[T]] {
      override def eqv(x: Relation[T], y: Relation[T]): Boolean =
        x.domain.equals(y.domain) && x.relation.equals(y.relation)
    }

  implicit def relationToGraph[T]: ToGraph.Aux[Relation[T], T] =
    new ToGraph[Relation[T]] {
      type Vertex = T

      def toGraph(t: Relation[Vertex]): G[Vertex] =
        G.vertices(t.domain.toList) + G.edges(t.relation.toList)

    }
  
}
