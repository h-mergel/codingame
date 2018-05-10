import scala.io.StdIn._
import scala.math._

/**
  * @author Heinrich Mergel
  **/
object Solution extends App {
  val coords = (parse(readLine), parse(readLine))
  var answer = Array.fill(readInt) { readLine split ";" }
    .map{ case Array(_,name,_,_,lon,lat) => dist(coords, (parse(lon),parse(lat))) -> name}
    .min._2
  println(answer)

  def dist (coords:(Double, Double), otherCoords:(Double, Double)) ={
    val x = (otherCoords._1 - coords._1)
    val y = (otherCoords._2 - coords._2)
    sqrt(pow(x,2) + pow(y,2)) * 6371
  }

  def parse = (s:String) => s.replace(",", ".").toDouble
}