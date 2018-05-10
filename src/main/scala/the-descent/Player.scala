import scala.io.StdIn._

/**
 * @author Heinrich Mergel
 **/
object Player extends App {
    while(true) {
        println((0 until 8).map((readInt, _)).max._2)
    }
}