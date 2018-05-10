import scala.io.StdIn._
import scala.util.matching.Regex

/**
  * @author Heinrich Mergel
  **/
object Solution extends App {

  val input = readLine.map(_.toBinaryString.reverse.padTo(7, 0).reverse.mkString).mkString
  print("""(0+)|(1+)""".r.findAllMatchIn(input)
      .map(x => s"${if (x.group(1) != null) "00" else "0"} ${"0" * x.toString().length}")
      .mkString(" "))

}