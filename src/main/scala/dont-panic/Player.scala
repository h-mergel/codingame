import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Player extends App {

  val Array(floorsN, width, roundsN, exitFloor, exitPos, clonesN, _, elevatorsN) = readLine.split(" ").map(_.toInt)
  val floorExits = Array.fill(elevatorsN){readLine.split(" ").map(_.toInt)}.map(a => a(0) -> a(1)).toMap + (exitFloor -> exitPos)

  def direction(x1:Int, x2:Int) = (x1 - x2) match {
    case 0 => "NONE"
    case x if(x > 0) => "LEFT"
    case x if(x < 0) => "RIGHT"
  }
  // game loop
  while (true) {
    val (cloneFloor, clonePos, cloneDirection) = readLine.split(" ")
    match { case Array(floor, pos, direct) => (floor.toInt, pos.toInt, direct) }

    cloneFloor match {
      case -1 => println("WAIT")
      case _ if(List("NONE", cloneDirection).contains(direction(clonePos, floorExits(cloneFloor)))) => println("WAIT")
      case _ => println("BLOCK");
    }
  }
}