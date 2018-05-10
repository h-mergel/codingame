import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Player extends App {
  var Array(x1, y1, x2, y2) = Array(0, 0) ++ readLine.split(" ").map(_.toInt)
  val n = readInt
  val startPos: (Int, Int) = readLine.split(" ").map(_.toInt) match { case Array(x, y) => (x, y)}

  def search(pos: (Int, Int)): Unit = {
    readLine.toCharArray.foreach(
      _ match {
        case 'U' => y2 = pos._2 - 1
        case 'R' => x1 = pos._1 + 1
        case 'D' => y1 = pos._2 + 1
        case 'L' => x2 = pos._1 - 1
      }
    )
    val newPos: (Int, Int) = ((x1 + x2) / 2, (y1 + y2) / 2)
    println(newPos._1 + " " + newPos._2)
    search(newPos)
  }

  search(startPos)
}