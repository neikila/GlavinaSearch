import geometry.GeometrySupport

/**
  * Created by k.neyman on 17.11.2017. 
  */
object Main extends GeometrySupport {
  def main(args: Array[String]): Unit = {
  }


  trait Generator[T] {
    def generate: T
  }
}














