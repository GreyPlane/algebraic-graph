import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import cats._
import cats.implicits._
import cats.derived.semiauto

import scala.collection.immutable.TreeMap

case class Branch(toStep: Int, criteria: List[Int])
case class StepLike(name: String, stepType: String, criteria: List[Int]) {
  override def toString() = {
    name
  }
}
case class Step(id: Int, stepType: String, branch: List[Branch])

class Test extends AnyWordSpecLike with Matchers {

  implicit val branchEq: Order[Branch] = semiauto.order
  implicit val stepLikeEq: Order[StepLike] = semiauto.order
  implicit val stepEq: Order[Step] = semiauto.order

//  val sss = 1 :: String :: HNil
//  sss.map
  "it should work" in {
    import AlgebraicGraph._
    import AlgebraicGraph.Graph._
    import AlgebraicGraph.implicits._
    import AlgebraicGraph.syntax._
    import shapeless._
    import shapeless.newtype._

    val propertyStep = StepLike("has-openid-branch", "branch", List.empty)
    val propertyStep2 = StepLike("has-mobile-branch", "branch", List.empty)
    val done = StepLike("done", "done", List(1))
    val done2 = StepLike("done-mobile-branch", "done", List(1))
    val sendSmsAction = StepLike("sendSms", "action", List(2))
    val sendTemplateMessageAction =
      StepLike("sendTemplateMessage", "action", List(2))

    val stars = List(
      List(propertyStep, sendSmsAction, propertyStep2),
      List(propertyStep2, sendTemplateMessageAction, done)
    )
      .map(vs => star(vs.head, vs.tail))
    val g3 = overlay(stars.head, stars(1))
    val graph = overlays(stars)
//    val graph = star(propertyStep, List(sendTemplateMessageAction, done))
    val vs = TreeMap.from(graph.vertexSet.zipWithIndex)

    val indexedGraph = graph.map(s => s -> vs(s))

    val s1 = graph.toAdjacencyMap
    val s2 = g3.toAdjacencyMap
    val sss = g3.show

    val steps = indexedGraph.toAdjacencyMap.adjacencyMap.map {
      case ((StepLike(name, stepType, criteria), i), branches) =>
        Step(
          i,
          stepType,
          branch = branches.map {
            case (StepLike(name, stepType, criteria), i) =>
              Branch(i, criteria)
          }.toList
        )
    }.toList

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
