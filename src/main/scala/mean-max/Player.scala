import math._
import scala.io.StdIn._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {


  val min = (a: Int, b: Int) => a < b
  val max = (a: Int, b: Int) => a > b

  // game loop
  while (true) {
    val myscore = readInt
    val enemyscore1 = readInt
    val enemyscore2 = readInt
    val myrage = readInt
    val enemyrage1 = readInt
    val enemyrage2 = readInt
    val unitcount = readInt

    val units: List[Unit] =
      for {_ <- List.range(0, unitcount)}
        yield {
          readLine split " "
          match {
            case Array(unitid, unittype, player, mass, radius, x, y, vx, vy, extra, extra2)
            => Unit(unitid.toInt, unittype.toInt, player.toInt, mass.toFloat, radius.toInt, (x.toInt, y.toInt), (vx.toInt, vy.toInt), extra.toInt, extra2.toInt)
          }
        }

    // debug
    units.foreach(Console.err.println)

    // predicates
    def combine[A](ps: (A => Boolean)*)(op: (Boolean, Boolean) => Boolean) = (a: A) => ps.map(_ (a)).reduce(op)
    def invert[A](p: A => Boolean): A => Boolean = (x) => !p(x)

    def isType(unittypes: Type*): Unit => Boolean = (unit: Unit) => unittypes.toArray.map(unittype => unittype.id).contains(unit.unittype)
    def isNotType(unittypes: Type*): Unit => Boolean = (unit: Unit) => invert(isType())(unit)
    def isPlayer(player: Int = 0): Unit => Boolean = _.player == player
    def isEnemy: Unit => Boolean = 1 to 2 contains _.player
    def isLeading: Unit => Boolean = _.player == (List(enemyscore1, enemyscore2).zipWithIndex.maxBy(_._1)._2 + 1)

    // search methods
    def filteredUnits(predicates: (Unit => Boolean)*) : List[Unit] = units.filter(combine(predicates: _*)(_ && _))
    def findUnit(coords: (Int, Int), unitList: List[Unit], fn: (Int, Int) => Boolean = min): Option[Unit] = unitList match {
      case Nil => None
      case List(x: Unit) => Some(x)
      case x :: y :: rest => findUnit(coords, (if (fn(distance(coords, x.pos), distance(coords, y.pos))) x else y) :: rest, fn)
    }

    // my units
    val reaper = filteredUnits(isPlayer(), isType(REAPER)).head
    val destroyer = filteredUnits(isPlayer(), isType(DESTROYER)).head
    val doof = filteredUnits(isPlayer(), isType(DOOF)).head

    // enemy units
    val enemyReaper1 = filteredUnits(isPlayer(1), isType(REAPER)).head
    val enemyReaper2 = filteredUnits(isPlayer(2), isType(REAPER)).head
    val leadingEnemy = filteredUnits(isEnemy, isType(REAPER), isLeading).head

    def score(unit: Unit): Int = {
      var result = 0

      // water
      result += unit.extra * 200

      // enemy distance
      val myDistance = distance(reaper.pos, unit.pos)
      val distanceScore = myDistance.toDouble / (distance(enemyReaper1.pos, unit.pos) + distance(enemyReaper2.pos, unit.pos))
      result -= (distanceScore * 10).toInt

      // my distance
      result -= myDistance

      val oilDistance = findUnit(unit.pos, filteredUnits(isType(OIL))).map(oil => distance(unit.pos, oil.pos)).getOrElse(Int.MaxValue)
      result -= (if (oilDistance < 1000) 10000 else 0)

      Console.err.println(s"EnemyDistance: ${distanceScore} MyDistance: ${myDistance} Water: ${unit.extra} OilDistance: ${oilDistance}")

      result
    }

    // REAPER
    val reaperTarget = filteredUnits(isType(WRECK)).map(u => u -> score(u)).sortWith(_._2 > _._2).headOption
      .map(_._1)
      .orElse(findUnit(reaper.pos, filteredUnits(isType(TANKER))))
    println(reaperTarget.map(u => {
      val thrust = if (distance(reaper.pos, u.pos) < 2000) 200 else 300
      s"${u.pos._1 - u.velocity._1} ${u.pos._2 - u.velocity._2} ${thrust}"
    }).getOrElse("WAIT"))

    // DESTROYER
    val destroyerTarget = findUnit(reaper.pos, filteredUnits(isType(TANKER)))
    val destroyerCommand = (if (myrage >= 30) findUnit(destroyer.pos, filteredUnits(isType(WRECK))) else None)
      .filter(w => distance(w.pos, leadingEnemy.pos) < 1000)
      .map(w => s"SKILL ${w.pos._1} ${w.pos._2}")
      .getOrElse(destroyerTarget.map(t => s"${t.pos._1} ${t.pos._2} 300").getOrElse("WAIT"))
    println(destroyerCommand)

    // DOOF
    val doofTarget = leadingEnemy
    val doofCommand = (if (myrage >= 30) findUnit(doof.pos, filteredUnits(isType(WRECK))) else None)
      .filter(w => distance(w.pos, leadingEnemy.pos)< 400)
      .map(w => s"SKILL ${w.pos._1} ${w.pos._2}")
      .getOrElse(s"${doofTarget.pos._1} ${doofTarget.pos._2} 300")
    println(doofCommand)
  }

  def distance(coords: (Int, Int), otherCoords: (Int, Int) = (0, 0)): Int = {
    new Vector(coords, otherCoords).mag.toInt
  }

  def angle(coords: (Int, Int), otherCoords: (Int, Int)): Int ={
    new Vector(coords, otherCoords).angle.toInt
  }
}
case class Unit(unitid: Int, unittype: Int, player: Int, mass: Float, radius: Int, pos: (Int, Int), velocity: (Int, Int), extra: Int, extra2: Int)

sealed abstract class Type(val id: Int, val throttle: (Int, Int) = (0, 0), val mass: Float = 0, val friction: Float = 0) extends Ordered[Type] {
  def compare(that: Type) = this.id - that.id
}
case object REAPER extends Type(0,  (0, 300), 0.5f, 0.2f)
case object DESTROYER extends Type(1, (0, 300), 1.5f, 0.3f)
case object DOOF extends Type(2,  (0, 300), 1f, 0.25f)
case object TANKER extends Type(3,  (500, 500), 2.5f, 0.4f)
case object WRECK extends Type(4 )
case object OIL extends Type(6)

// companion object
object Const {
  val SKILL_RANGE = 2000
  val SKILL_RAD = 1000
}

case class Vector (val x: Double, val y:Double) {

  def this(coords: (Int, Int), otherCoords: (Int, Int)) = this(otherCoords._1 - coords._1, otherCoords._2 - coords._2)

  def mag = sqrt(pow(x, 2) + pow(y, 2))
  def angle = toDegrees(atan2(y, x))
  def dotProduct(other: Vector): Double = x * other.x + y * other.y
  def angle(other: Vector): Double = toDegrees(acos(dotProduct(other) / (mag * other.mag)))
}