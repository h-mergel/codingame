import scala.io.StdIn._
import Math._

/**
  * Magic numbers everywhere!!! :)
  * @author Heinrich Mergel
  */
object Player extends App {
  val surfaceN = readInt // the number of points used to draw the surface of Mars.

  val surface = Array.fill(surfaceN)(readLine.split(" ").map(_.toInt))
  val landingArea = surface.sliding(2).find(points => points(0)(1) == points(1)(1)).get

  while (true) {
    val lander = readLine.split(" ").map(_.toInt)
    match {
      case Array(x, y, hspeed, vspeed, fuel, rotate, power)
      => Lander((x, y), (hspeed, vspeed), fuel, rotate, power)
    }

    val hSpeed = lander.velocity._1
    val nextLanderPos = lander.pos._1 + hSpeed * 10
    val isInLandingArea = nextLanderPos > landingArea(0)(0) + 200 && nextLanderPos < landingArea(1)(0) - 200
    val rotation = if (isInLandingArea && (hSpeed > 5 || hSpeed < -5))
      hSpeed.max(-60).min(60)
    else if (hSpeed.abs > 80)
      hSpeed.max(-15).min(15)
    else if (!isInLandingArea && hSpeed.abs < 60)
      (nextLanderPos - (landingArea(1)(0) + landingArea(0)(0)) / 2).max(-20).min(20)
    else 0

    val vSpeed = lander.velocity._2
    val threshold = if (isInLandingArea) -35 else -10
    val thrust = if (vSpeed < threshold
      || !isInLandingArea && lander.pos._2 < landingArea(0)(1) + 300
      ||rotation != 0) 4
    else 0

    println(s"$rotation $thrust")
  }
}

case class Lander(pos: (Int, Int), velocity: (Int, Int), fuel: Int, rotate: Int, power: Int)