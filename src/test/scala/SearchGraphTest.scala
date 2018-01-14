import geometry._
import geometry.task.{AccuracySettings, ConnectionStatusUpdater, Node, SearchGraph}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class SearchGraphTest extends FunSuite {

  test("Save new targets") {
    val root = new Point(0, 0)
    val finish = new Point(10, 10)
    val target = new Point(1, 1)

    val graph = new SearchGraph(root, finish, new AccuracySettings)
    graph.connect(target, root, Nil)

    assert(graph.getSubTargets == target :: Nil)
    assert(!graph.hasFoundWay)
  }

  test("Understand that path was found") {
    val root = new Point(0, 0)
    val finish = new Point(10, 10)
    val target = new Point(1, 1)

    val graph = new SearchGraph(root, finish, new AccuracySettings)
    graph.connect(target, root, Nil)
    assert(!graph.hasFoundWay)
    graph.connect(target, finish, Nil)

    assert(graph.getSubTargets == target :: Nil)
    assert(graph.hasFoundWay)
  }
}
