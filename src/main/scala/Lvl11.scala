import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object Lvl11 extends App {
  val seatLayout = Source.fromResource("lvl11.txt").getLines().toSeq.map(_.split("").toSeq)
  val twoDimIndices = seatLayout.indices.flatMap(row => seatLayout.head.indices.map(col => (row, col)))

  @tailrec
  def iterateToEquilibrium[T](f: T => T)(old: T): T = {
    val newVal = f(old)
    if (newVal == old) old else iterateToEquilibrium(f)(newVal)
  }

  // part 1

  def part1Rules(oldLayout: Seq[Seq[String]]): Seq[Seq[String]] = {
    oldLayout.zipWithIndex.par.map { case (line, row) =>
      line.zipWithIndex.map { case (seat, col) =>
        val surroundingSeats = for {
          i <- -1 to 1
          j <- -1 to 1
          if !(i == 0 && j == 0)
          if oldLayout.indices.contains(row + i)
          if oldLayout.head.indices.contains(col + j)
        } yield (row + i, col + j)

        seat match {
          case "." => "."
          case "L" =>
            if (surroundingSeats.exists(idx => oldLayout(idx._1)(idx._2) == "#")) "L" else "#"
          case "#" =>
            if (surroundingSeats.count { idx => oldLayout(idx._1)(idx._2) == "#" } >= 4) "L" else "#"
        }
      }
    }.seq
  }

  def numOccupied(layout: Seq[Seq[String]]): Int = layout.flatten.count(_ == "#")

  // part 2
  def part2Rules(oldLayout: Seq[Seq[String]]): Seq[Seq[String]] =
    oldLayout.zipWithIndex.par.map { case (line, row) =>
      line.zipWithIndex.map { case (seat, col) =>
        val directionsToCheck = for {
          i <- -1 to 1
          j <- -1 to 1
          if !(i == 0 && j == 0)
          if oldLayout.indices.contains(row + i)
          if oldLayout.head.indices.contains(col + j)
        } yield (i, j)

        seat match {
          case "." => "."
          case "L" =>
            if (directionsToCheck.exists(dir => !checkDirectionEmpty(oldLayout, (row, col), dir))) "L" else "#"
          case "#" =>
            if (directionsToCheck.count(dir => !checkDirectionEmpty(oldLayout, (row, col), dir)) >= 5) "L" else "#"
        }
      }
    }.seq

  @tailrec
  def checkDirectionEmpty(layout: Seq[Seq[String]], start: (Int, Int), direction: (Int, Int)): Boolean = {
    val newPos = (start._1 + direction._1, start._2 + direction._2)
    layout.lift(newPos._1).flatMap(_.lift(newPos._2)) match {
      case Some("L") => true
      case Some("#") => false
      case Some(".") => checkDirectionEmpty(layout, newPos, direction)
      case Some(x) => throw new RuntimeException(s"Wrong input, did not expect to find '$x'")
      case None => true
    }
  }

  val part1Equlibrium = iterateToEquilibrium(part1Rules)(seatLayout)
  println(s"Solution 1 is: ${numOccupied(part1Equlibrium)}")


  val part2Equlibrium = iterateToEquilibrium(part2Rules)(seatLayout)
  println(s"Solution 2 is: ${numOccupied(part2Equlibrium)}")

}
