import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  */
object Solution extends App {
  print(Array.fill[Int](readInt()){readInt()}.sorted.sliding(2).map(x => x(1) - x(0)).min)
}