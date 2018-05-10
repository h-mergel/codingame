import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Solution extends App {
  val cardsP1 = List.fill(readInt)(readLine.charAt(0))
  val cardsP2 = List.fill(readInt)(readLine.charAt(0))
  val order = Array('2', '3', '4', '5', '6', '7', '8', '9', '1', 'J', 'Q', 'K', 'A')


  def fight(cards1: List[Char], cards2: List[Char], round: Int): (List[Char], List[Char], Int) = {
    if (cards1.isEmpty || cards2.isEmpty)
      (cards1, cards2, round)
    else {
      val c1 = cards1.head
      val c2 = cards2.head
      (order.indexOf(c1) - order.indexOf(c2)) match {
        case x if (x > 0) => fight(cards1.tail ::: List(c1, c2), cards2.tail, round + 1)
        case x if (x < 0) => fight(cards1.tail, cards2.tail ::: List(c1, c2), round + 1)
        case 0 => war(cards1, cards2, round, 0)
      }
    }
  }

  def war(cards1: List[Char], cards2: List[Char], round: Int, warRound: Int): (List[Char], List[Char], Int) = {
    val warLength = 4 * warRound
    if (cards1.length < warLength) {
      (List.empty, cards2 ::: cards1, -1)
    } else if (cards2.length < warLength) {
      (cards1 ::: cards2, List.empty, -1)
    } else {
      val (war1, tail1) = cards1.splitAt(warLength)
      val (war2, tail2) = cards2.splitAt(warLength)
      val c1 = tail1.head
      val c2 = tail2.head
      (order.indexOf(c1) - order.indexOf(c2)) match {
        case x if (x > 0) => fight(tail1.tail ::: war1 ::: List(c1) ::: war2 ::: List(c2), tail2.tail, round + 1)
        case x if (x < 0) => fight(tail1.tail, tail2.tail ::: war1 ::: List(c1) ::: war2 ::: List(c2), round + 1)
        case 0 => war(cards1, cards2, round, warRound + 1)
      }
    }
  }

  val res = fight(cardsP1, cardsP2, 0)
  if (res._3 == -1) println("PAT") else println((if (res._1.isEmpty) 2 else 1) + " " + res._3)
}