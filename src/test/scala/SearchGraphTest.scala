import geometry._
import labtask.{AccuracySettings, ConnectionStatusUpdater, Node, SearchGraph}
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

  test("Can build result path") {
    val root = new Point(0, 0)
    val afterRoot = new Point(2, 2)
    val afterRoot2 = new Point(4, 4)

    val target = new Point(5, 5)

    val beforeFinish2 = new Point(10, 10)
    val beforeFinish1 = new Point(11, 11)
    val finish = new Point(12, 12)

    val uselessPoint = new Point(0, 100)

    val graph = new SearchGraph(root, finish, new AccuracySettings)
    graph.connect(afterRoot2, root, MyVector(root, afterRoot) :: MyVector(afterRoot, afterRoot2) :: Nil)
    graph.connect(beforeFinish2, finish, MyVector(finish, beforeFinish1) :: MyVector(beforeFinish1, beforeFinish2) :: Nil)
    graph.connect(target, afterRoot2, MyVector(afterRoot2, target) :: Nil)
    graph.connect(uselessPoint, finish, MyVector(finish, target) :: Nil)
    assert(!graph.hasFoundWay)
    graph.connect(target, beforeFinish2, MyVector(beforeFinish2, target) :: Nil)

    assert(graph.hasFoundWay)
    assert(graph.connectionNode.get.point == target)
    val points = root :: afterRoot :: afterRoot2 :: target :: beforeFinish2 :: beforeFinish1 :: finish :: Nil

    val idealPath = (points zip points.drop(1)).map { case (l, r) => MyVector(l, r) }
    assert(graph.getPath == idealPath)
  }
}
