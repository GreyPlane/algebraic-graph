package ag

import ag.AlgebraicGraph.*
trait ToGraph[T] {
  type Vertex

  def toGraph(t: T): Graph[Vertex]

  def foldg[R](e: R, v: Vertex => R, o: (R, R) => R, c: (R, R) => R)(t: T) =
    toGraph(t).foldg(e, v, o, c)
}

object ToGraph {
  type Aux[T, V] = ToGraph[T] { type Vertex = V }

  def apply[T, V](using tg: ToGraph.Aux[T, V]): Aux[T, V] = tg
}
