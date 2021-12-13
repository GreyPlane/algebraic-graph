
trait ToGraph[T] {
  type Vertex

  def toGraph(t: T): AlgGraphHK.Graph[Vertex]
  //  = AlgGraphHK.foldg(Empty, Vertex[V], Overlay[V], Connect[V])(g)

  //  foldg :: r -> (ToVertex t -> r) -> (r -> r -> r) -> (r -> r -> r) -> t -> r
  def foldg[R](e: R, v: Vertex => R, o: (R, R) => R, c: (R, R) => R)(t: T) = AlgGraphHK.foldg(e, v, o, c)(toGraph(t))
}

object ToGraph {
  type Aux[T, V] = ToGraph[T] { type Vertex = V }

  def apply[T, V](implicit tg: ToGraph.Aux[T, V]): Aux[T, V] = tg
}