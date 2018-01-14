import geometry._
import geometry.support.GeometrySupport
import geometry.task.{AccuracySettings, ConnectionStatusUpdater, Node}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class ConnectionStatusUpdaterTest extends FunSuite {

//  test("1") {
//    assert(1 == 1)
//  }

  test("Connect start with finish") {
    val root = Node.root(Point(0, 0))
    val finish = Node.finish(Point(10, 10))

    root.addNeighbor(finish, Nil)
    finish.addNeighbor(root, Nil)

    new ConnectionStatusUpdater(root, finish).update()

    assert(root.isConnectedToFinish && root.isConnectedToStart)
    assert(finish.isConnectedToFinish && finish.isConnectedToStart)
  }

  test("Connect start with middle") {
    val root = Node.root(Point(0, 0))
    val mid = new Node(Point(1, 1))

    root.addNeighbor(mid, Nil)
    mid.addNeighbor(root, Nil)

    new ConnectionStatusUpdater(root, mid).update()

    assert(!mid.isConnectedToFinish && mid.isConnectedToStart)
  }

  test("Connect two chains") {
    val root = Node.root(Point(0, 0))
    val afterRoot1 = new Node(Point(1, 1))
    val afterRoot2 = new Node(Point(2, 2))
    afterRoot1.isConnectedToStart = true
    afterRoot2.isConnectedToStart = true
    connect(root, afterRoot1)
    connect(afterRoot1, afterRoot2)

    val finish = Node.finish(Point(3, 3))
    val beforeFinish1 = new Node(Point(4, 4))
    val beforeFinish2 = new Node(Point(5, 5))
    beforeFinish1.isConnectedToFinish = true
    beforeFinish2.isConnectedToFinish = true
    connect(finish, beforeFinish1)
    connect(beforeFinish1, beforeFinish2)

    connect(afterRoot2, beforeFinish2)
    new ConnectionStatusUpdater(afterRoot2, beforeFinish2).update()

    assert(root.isConnectedToFinish && root.isConnectedToStart)
    assert(afterRoot1.isConnectedToFinish && afterRoot1.isConnectedToStart)
    assert(afterRoot2.isConnectedToFinish && afterRoot2.isConnectedToStart)

    assert(finish.isConnectedToFinish && finish.isConnectedToStart)
    assert(beforeFinish1.isConnectedToFinish && beforeFinish1.isConnectedToStart)
    assert(beforeFinish2.isConnectedToFinish && beforeFinish2.isConnectedToStart)
  }

  private def connect(node1: Node, node2: Node) = {
    node1.addNeighbor(node2, Nil)
    node2.addNeighbor(node1, Nil)
  }
}
