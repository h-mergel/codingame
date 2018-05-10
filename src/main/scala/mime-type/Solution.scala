import scala.io.StdIn._

/**
 * @author Heinrich Mergel
 **/
object Solution extends App {
  val n = readInt // Number of elements which make up the association table.
  val q = readInt // Number Q of file names to be analyzed.

  var mimeTypes = Array.fill(n) { readLine split " " }.map { case Array(a, b) => a.toLowerCase() -> b }.toMap
  val pattern = """(.*)[.](.*)""".r
  for(i <- 0 until q) println(mimeTypes.getOrElse(pattern.findFirstMatchIn(readLine).map {_.group(2).toLowerCase}.getOrElse("UNKNOWN"), "UNKNOWN"))

}