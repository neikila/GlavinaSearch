package geometry.task

import geometry.{MyVector, Point}
import geometry.Point.PointAccuracyEqual

/**
  * Created by Neikila on 14.01.2018.
  */
class SearchGraph(startPoint: Point, endPoint: Point, accuracySettings: AccuracySettings) {
  implicit val POINT_ACCURACY_EQUAL: PointAccuracyEqual = accuracySettings.POINT_ACCURACY_EQUAL

  private val startNode = Node.root(startPoint)
  private val endNode = Node.finish(endPoint)

  private var subTargets: List[Node] = Nil

  def connect(target: Point, achievedFrom: Point, path: List[MyVector]): Unit = {
    val fromNode = findNode(achievedFrom)

    val nodeToConnect = subTargets.find(_.point.isApproximatelyEqual(target)) match {
      case Some(p) => p
      case None =>
        val newNode = new Node(target)
        subTargets = newNode :: subTargets
        newNode
    }

    connect(fromNode, nodeToConnect, path)
    updateConnectionStatus(nodeToConnect, fromNode)
  }

  def getSubTargets: List[Point] = {
    subTargets.map(_.point)
  }

  private def updateConnectionStatus(child: Node, parent: Node): Unit = {
    child.isConnectedToFinish = child.isConnectedToFinish || parent.isConnectedToFinish
    child.isConnectedToStart = child.isConnectedToStart || parent.isConnectedToStart
  }

  private def findNode(point: Point): Node = {
    (startNode :: endNode :: subTargets)
      .find(_.point.isApproximatelyEqual(point))
      .getOrElse(throw new IllegalArgumentException(s"No node near $point"))
  }

  private def connect(node1: Node, node2: Node, pathFrom1To2: List[MyVector]): Unit = {
    node1.addNeighbor(node2, pathFrom1To2)
    node2.addNeighbor(node1, reversePath(pathFrom1To2))
  }

  private def reversePath(path: List[MyVector]): List[MyVector] = {
    path
  }

}

class Node(val point: Point) {
  var neighbours: List[NodeNeighbor] = Nil
  var isConnectedToStart = false
  var isConnectedToFinish = false

  def addNeighbor(node: Node, path: => List[MyVector]): Unit = {
    if (!neighbours.exists(_.nodeNeighbor == node)) {
      neighbours = NodeNeighbor(node, path) :: neighbours
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that: Node => point == that.point
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(point)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Node {
  def root(point: Point): Node = {
    val node = new Node(point)
    node.isConnectedToStart = true
    node
  }

  def finish(point: Point): Node = {
    val node = new Node(point)
    node.isConnectedToFinish = true
    node
  }
}

case class NodeNeighbor(nodeNeighbor: Node, pathToNeighbor: List[MyVector])

class ConnectionStatusUpdater(val nodeConnected1: Node, val nodeConnected2: Node) {
  private val newIsConnectedToStart = nodeConnected1.isConnectedToStart || nodeConnected2.isConnectedToStart
  private val newIsConnectedToFinish = nodeConnected1.isConnectedToFinish || nodeConnected2.isConnectedToFinish

  def update(): Unit = {
    updateNode(nodeConnected1)
    updateNeighborsOf(nodeConnected1 :: Nil)
  }

  private def updateNeighborsOf(front: List[Node]): Unit = {
    val list: List[Node] = front.flatMap(_.neighbours.map(_.nodeNeighbor))
    list.filter(updateNode) match {
      case Nil => None
      case nodes => updateNeighborsOf(nodes)
    }
  }

  private def updateNode(node: Node): Boolean = {
    val isUpdated = newIsConnectedToStart != node.isConnectedToStart || newIsConnectedToFinish != node.isConnectedToFinish
    node.isConnectedToFinish = node.isConnectedToFinish || newIsConnectedToFinish
    node.isConnectedToStart = node.isConnectedToStart || newIsConnectedToStart
    isUpdated
  }
}
