import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import cats._
import cats.implicits._
import cats.derived.semiauto

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

class DefaultDict[K, V] {
  private val map: mutable.Map[K, V] = mutable.Map.empty[K, V]
  def get(key: K): Either[V, DefaultDict[K, V]] = {
    map.get(key).map(Left.apply).getOrElse(Right(this))
  }
}

case class Branch(toStep: Int, criteria: Int)
case class StepLike(name: String, stepType: String, criteria: Int) {
  override def toString() = {
    name
  }
}
case class Step(id: Int, stepType: String, branch: List[Branch])

case class Strategy(name: String, criteria: List[Int], action: String)

class Test extends AnyWordSpecLike with Matchers {

  import AlgebraicGraph._
  import AlgebraicGraph.Graph._
  import AlgebraicGraph.implicits._
  import AlgebraicGraph.syntax._
  import shapeless.newtype._

  implicit val branchEq: Order[Branch] = semiauto.order
  implicit val stepLikeEq: Order[StepLike] = semiauto.order
  implicit val stepEq: Order[Step] = semiauto.order

  def transform(strategies: List[Strategy]): (StepLike, Graph[StepLike]) =
    strategies match {
      case head :: Nil => {
        val level = head.name
        val cs = head.criteria
        val steps = cs.zipWithIndex.flatMap { case (c, i) =>
          i match {
            case n if (n == cs.length) =>
              List(
                StepLike(s"$level-$c-branch", "branch", c),
                StepLike(s"$level-$c-action", head.action, -2),
                StepLike(s"$level-$c-done", "done", -1)
              )
            case 0 => List(StepLike(s"$level-$c-branch", "branch", c))
            case _ =>
              List(
                StepLike(s"$level-$c-branch", "branch", c),
                StepLike(s"$level-$c-done", "done", -1)
              )
          }
        }
        val graph =
          if (cs.length == 1) star(steps.head, steps.tail)
          else
            vertex(steps.head) * vertex(steps.tail.head) + ladder(
              steps.tail,
              2
            )
        steps.head -> graph
      }
      case head :: next => {
        val (root, graph) = transform(List(head))
        val (subGraphRoot, subGraph) = transform(next)
        root -> (vertex(root) * vertex(subGraphRoot) + graph + subGraph)
      }
      case Nil => StepLike("", "", 0) -> Graph.empty[StepLike]
    }

  "it should work" in {

    val propertyStep = StepLike("has-openid-branch", "branch", 0)
    val propertyStep2 = StepLike("has-mobile-branch", "branch", 0)
    val done = StepLike("done", "done", 0)
    val done2 = StepLike("done-mobile-branch", "done", 0)
    val sendSmsAction = StepLike("sendSms", "action", 0)
    val sendTemplateMessageAction =
      StepLike("sendTemplateMessage", "action", 0)

    val sts = List(
      Strategy("Java", List(11, 22, 33), "toGroovy"),
      Strategy("Groovy", List(11, 22, 33), "toScala"),
      Strategy("Scala", List(11, 22), "Scala ftw!")
    )

    val (rootStep, gnn) = transform(sts)
    val gnnn = gnn.toAdjacencyMap

    val stars = List(
      List(propertyStep, sendSmsAction, propertyStep2),
      List(propertyStep2, sendTemplateMessageAction, done)
    )
      .map(vs => star(vs.head, vs.tail))

    val rawSteps = List(
      propertyStep,
      sendSmsAction,
      propertyStep2,
      sendTemplateMessageAction,
      done
    )
    val g4 = ladder(rawSteps, 2)
    val g3 = overlay(stars.head, stars(1))
    val ggg = g4.toAdjacencyMap
    val graph = overlays(stars)
//    val graph = star(propertyStep, List(sendTemplateMessageAction, done))
    val vs = TreeMap.from(graph.vertexSet.zipWithIndex)

    val indexedGraph = graph.map(s => s -> vs(s))

    val m = path(Range.inclusive(1, 9).toList) + path(List(9, 1)) + star(5, List(2, 4, 6, 8))
    val adm = m.toAdjacencyMap
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
    type Matrix = List[List[Option[Int]]]

    def isAdjacency(from: Int, to: Int): Boolean = {
      ???
    }

    def move(matrix: Matrix, from: Int, to: Int): Option[Matrix] = {
      if (isAdjacency(from, to)) {
        matrix.lift
        ???
      } else None
    }

    1 shouldBe 1
  }
}
