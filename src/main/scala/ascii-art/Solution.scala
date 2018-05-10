import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Solution extends App {

  val l = readInt
  val h = readInt
  val t = readLine.toUpperCase()
  for (i <- 0 until h) {
    val row = readLine
    t.map(c => if(c.isLetter) c - 'A' else 26).foreach(c => print(row.substring(c * l, (c + 1) * l)))
    println()
  }
}