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

  var hasFoundWay = false

  def getSubTargets: List[Point] = {
    subTargets.map(_.point)
  }

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
    hasFoundWay |= new ConnectionStatusUpdater(fromNode, nodeToConnect).update()
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
    path.reverse.map { case MyVector(from, to) => MyVector(to, from) }
  }
}








