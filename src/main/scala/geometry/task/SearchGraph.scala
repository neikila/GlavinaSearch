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

  var connectionNode: Option[Node] = None
  def hasFoundWay: Boolean = connectionNode.isDefined
  def getPath: List[MyVector] = {

    var lastNode: Option[Node] = connectionNode
    var pathToFinish: List[Node] = Nil
    do {
      pathToFinish = lastNode.get :: pathToFinish
      lastNode = lastNode.get.parentToFinish
    } while (lastNode.isDefined)

    lastNode = connectionNode.get.parentToStart
    var pathToStart: List[Node] = Nil
    do {
      pathToStart = lastNode.get :: pathToStart
      lastNode = lastNode.get.parentToStart
    } while (lastNode.isDefined)

    val path = pathToStart ::: pathToFinish.reverse
    println("Path")
    println(path.mkString("\n"))
    println()
    (path zip path.drop(1)).flatMap { case (left, right) => left.neighbours.find(_.nodeNeighbor == right).get.pathToNeighbor }
  }

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
    if (fromNode.isConnectedToStart) nodeToConnect.parentToStart = fromNode
    if (fromNode.isConnectedToFinish) nodeToConnect.parentToFinish = fromNode
    if (new ConnectionStatusUpdater(fromNode, nodeToConnect).update() && !hasFoundWay) {
      connectionNode = Some(nodeToConnect)
    }
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








