import scala.annotation.tailrec

object Lvl15 {

  def findNewNumber(lastMentionedAt: Map[Int, Int], number: Int, iteration: Int): Int = {
    lastMentionedAt.get(number) match {
      case Some(lastSeen) => iteration - lastSeen
      case None => 0
    }
  }

  @tailrec
  def game(lastMentionedAt: Map[Int, Int], lastNumber: Int, returnIndex: Int, iteration: Int) : Int = {
    val newNumber = findNewNumber(lastMentionedAt, lastNumber, iteration - 1)
    if (iteration == returnIndex) return newNumber
    game(lastMentionedAt + (lastNumber -> (iteration - 1)), newNumber, returnIndex, iteration + 1)
  }

  def startGameWith(startingNumbers: Seq[Int], returnIndex: Int) : Int = {
    val lastMentionedAt = startingNumbers.dropRight(1).zipWithIndex.map(x => (x._1, x._2 + 1)).toMap
    game(lastMentionedAt, startingNumbers.last, returnIndex, startingNumbers.length + 1)
  }

  def main(args: Array[String]): Unit = {
    val input = "6,3,15,13,1,0".split(",").toSeq.map(_.toInt)

    val result1 = startGameWith(input, 30000000)

    println(s"Solution to part 1: $result1")
  }
}
