import Tests.{CrossDetections, FigureSupport}
import geometry.GeometrySupport

/**
  * Created by k.neyman on 17.11.2017. 
  */
object Main extends GeometrySupport {
  def main(args: Array[String]): Unit = {
//    new TestRunner().run()
//    new Tests.TestSearchWay().testSearchWithDeadEnd()
    new Tests.TestSearchWay().testSimpleWayBadNums()
//    new Tests.FigureSupport().testCrossedByVector2()
//    new Tests.OptimiserTest().testSearch()
//    new Tests.CrossDetections().testCrossingStrangeVals()
  }


  trait Generator[T] {
    def generate: T
  }

  private class TestRunner {
    def run(): Unit = {
      runFigureContainTests()
      runCrossDetectionTests()
    }

    def runFigureContainTests(): Unit = {
      println("FigureSupport")
      val support: FigureSupport = new Tests.FigureSupport()
      support.testNotContains()
      support.testContains()
      support.testContainsHorizontalLineCrossVertex()
      support.testNotContainsVertex()
      support.testNotContainsHorizontal()
    }

    def runCrossDetectionTests(): Unit = {
      println("FigureSupport")
      val detections: CrossDetections = new Tests.CrossDetections()
      detections.testContainsPoint()
      detections.testHalfBounds()
      detections.testCrossing()
      detections.testOneSharedPoint()
      detections.testParallel()
      detections.testSelfCheck()
    }
  }
}














