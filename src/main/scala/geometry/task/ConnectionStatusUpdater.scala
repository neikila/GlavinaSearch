package geometry.task

/**
  * Created by Neikila on 14.01.2018.
  */
class ConnectionStatusUpdater(val nodeConnected1: Node, val nodeConnected2: Node) {
  private val newIsConnectedToStart = nodeConnected1.isConnectedToStart || nodeConnected2.isConnectedToStart
  private val newIsConnectedToFinish = nodeConnected1.isConnectedToFinish || nodeConnected2.isConnectedToFinish

  def update(): Boolean = {
    updateNode(nodeConnected1)
    updateNeighborsOf(nodeConnected1 :: Nil)
    newIsConnectedToFinish && newIsConnectedToStart
  }

  private def updateNeighborsOf(initFront: List[Node]): Unit = {
    var front = initFront
    while (front.nonEmpty) {
      front = front
        .flatMap(_.neighbours.map(_.nodeNeighbor))
        .filter(updateNode)
    }
  }

  private def updateNode(node: Node): Boolean = {
    val isUpdated = newIsConnectedToStart != node.isConnectedToStart || newIsConnectedToFinish != node.isConnectedToFinish
    node.isConnectedToFinish = node.isConnectedToFinish || newIsConnectedToFinish
    node.isConnectedToStart = node.isConnectedToStart || newIsConnectedToStart
    isUpdated
  }
}
