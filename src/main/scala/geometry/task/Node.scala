package geometry.task

import geometry.{MyVector, Point}

/**
  * Created by Neikila on 14.01.2018.
  */
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