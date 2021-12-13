import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import cats._
import cats.implicits._
import cats.derived.semiauto

case class Branch(toStep: Int, criteria: List[Int])
case class StepLike(name: String, stepType: String, criteria: List[Int])
case class Step(id: Int, stepType: String, branch: List[Branch])

class Test extends AnyWordSpecLike with Matchers {

  implicit val branchEq: Order[Branch] = semiauto.order
  implicit val stepLikeEq: Order[StepLike] = semiauto.order
  implicit val stepEq: Order[Step] = semiauto.order

//  val sss = 1 :: String :: HNil
//  sss.map
  "it should work" in {
    import AlgGraphHK._
    import AlgGraphHK.Graph._
    import AlgGraphHK.implicits._
    import AlgGraphHK.syntax._
    import shapeless._
    import shapeless.newtype._

    val propertyStep = StepLike("branch-1", "branch", List.empty)
    val done = StepLike("done-1", "done", List(1))
    val sendSmsAction = StepLike("sendSms-1", "action", List(2))

    val graph = star(propertyStep, List(sendSmsAction, done))
    val vs = graph.vertexSet.zipWithIndex.toMap

    val indexedGraph = graph.map(s => s -> vs(s))

    val steps = indexedGraph.toAdjacencyMap.adjacencyMap
      .map { case ((StepLike(name, stepType, criteria), i), branches) =>
        Step(
          i,
          stepType,
          branch = branches.map {
            case (StepLike(name, stepType, criteria), i) =>
              Branch(i, criteria)
          }.toList
        )
      }

    Graph.vertex(1) + Graph.vertex(2) * Graph.vertex(3)
    val g = Graph
      .clique(List(1, 2, 3))
      .map(_ + 2)
      .flatMap(n => Graph.star(n, List(10, 11)))
      .mergeVertices(_ > 7, 1)
    val ges = g.edgeList
    val s = g.show
    val ad = g.toAdjacencyMap
    1 shouldBe 1
  }
  "ad" in {
    import Data._
    import Data.implicits._

    val a = 1.vertex
    val b = 2.vertex
    val e = 1.edge(2)
    val e2 = 1.edge(3)

    val c = AdjacencyMap.overlay(e, e2)
    val d = AdjacencyMap.connect(a, b)

    1 shouldBe 1
  }
}
