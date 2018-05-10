import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Player extends App {
    val W = readInt // the number of cells on the X axis
    val H = readInt // the number of cells on the Y axis

    val nodes = Array.fill(H)(readLine.toCharArray)
    for(row <- 0 until H; col <- 0 until W) {
        if(nodes(row)(col) == '0') {
            val (rx, ry) = nodes(row).zipWithIndex.find(n => n._1 == '0' && n._2 > col).map(n => (n._2, row)).getOrElse((-1, -1))
            val (bx, by) = nodes.map {_(col)}.zipWithIndex.find(n => n._1 == '0' && n._2 > row).map(n => (col, n._2)).getOrElse((-1, -1))
            println(s"$col $row $rx $ry $bx $by")
        }
    }
}