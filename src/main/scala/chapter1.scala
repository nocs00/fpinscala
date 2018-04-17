package chapter1


object Chapter1 extends App {

  case class Player(name: String, score: Int)

  def printWinner(p: Player): Unit =
    println(p.name + " is the winner!")

  def winner(p1: Player, p2: Player): Player =
    if (p1.score > p2.score) p1 else p2

  def declareWinner(p1: Player, p2: Player): Unit =
    printWinner(winner(p1, p2))

  def testWinners: Unit = {
    val players = List(
      Player("Sue", 7),
      Player("Bob", 8),
      Player("Joe", 4))
    val p = players.reduceLeft(winner)
    printWinner(p)
  }

}

object Runner {
  import Chapter1._

  def main(args: Array[String]): Unit = {
    testWinners
  }
}


