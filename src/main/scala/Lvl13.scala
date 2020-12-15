import scala.annotation.tailrec
import scala.io.Source

object Lvl13 extends App {
  val input = Source.fromResource("lvl13.txt").getLines().toSeq
  val earliestDeparture = input.head.toInt
  val busSchedule = input.last.split(",").toSeq


  def part1(departure: Int, schedule: Seq[String]): Int = {
    val busIds = schedule.filter(_ != "x").map(_.toInt).toSet
    val waitingTimes = busIds.map(id => (id, id - (departure % id)))
    val earliestBus = waitingTimes.minBy(_._2)
    earliestBus._1 * earliestBus._2
  }

  val result = part1(earliestDeparture, busSchedule)
  println(s"Part 1 solution: $result")

  //part 2

  val busOffsets = busSchedule.zipWithIndex.filter(_._1 != "x").map(x => (x._1.toInt, x._2))
  val startingT = BigInt(100000000L) * BigInt(1000000L) + 3


  def departWithOffsetsBruteForce(schedule: Seq[String], startFrom: BigInt = startingT): BigInt = {
    val possibleSolutions = LazyList.from(0).map(_ + startFrom).sliding(schedule.length).map(_.zip(schedule.map(_.toIntOption)))
    possibleSolutions.filter(_.forall{
      case (_, None) => true
      case (t, Some(i)) => t % i == 0
    }).next().head._1
  }

  // never finishes
  //println(departWithOffsetsBruteForce(busSchedule))


  def departWithOffsetsBruteForce2(schedule: Seq[String], startFrom: BigInt = startingT): BigInt = {
    val intSchedule = schedule.zipWithIndex.filter(_._1 != "x").map(x => (x._1.toInt, x._2))
    val largest = intSchedule.maxBy(_._1)
    val actualStartFrom = startFrom - startFrom % largest._1
    def possibleSolutions = LazyList.iterate(actualStartFrom)(_ + largest._1)
    possibleSolutions.filter(t => intSchedule.forall{case (bus, offset) => (t + offset - largest._2) % bus == 0}).head - largest._2
  }
  // faster than the first one but still not enough
  // println(departWithOffsetsBruteForce2(busSchedule))


  @tailrec
  def greatestCommonDivisor(a: Int, b: Int):Int=if (b==0) a.abs else greatestCommonDivisor(b, a%b)

  def leastCommonMultiple(a: Int, b: Int)=(a*b).abs/greatestCommonDivisor(a,b)

  /**Extended euclidian algorithm
   *
   * Taken from <a href="https://de.wikipedia.org/wiki/Erweiterter_euklidischer_Algorithmus#Algorithmische_Umsetzung">Wikipedia</a>.
   * Calculates the greatest commond divisor and the two numbers x, y so that <pre>a*x+b*y = gcd(a, b)</pre>
   *
   * Returns: Tuple(gcd(a, b), x, y)
   * */
  def extendedEuclid(a: BigInt, b: BigInt) : (BigInt, BigInt, BigInt) = {
    if (b == 0) return (a, 1, 0)
    val dst = extendedEuclid(b, a % b)
    (dst._1, dst._3, dst._2 - (a / b) * dst._3)
  }

  def mod_floor(a: BigInt, n: BigInt): BigInt = ((a % n) + n) % n

  def leastCommonMultipleWithOffset(aWithOffset: (BigInt, BigInt), bWithOffset: (BigInt, BigInt)) : (BigInt, BigInt) = {
    val (a, aOffset) = aWithOffset
    val (b, bOffset) = bWithOffset
    val bOffsetFromPerspectiveOfAOffset = (aOffset+bOffset) % b
    val (gcd, x, _) = extendedEuclid(a, b)
    val lcm = a * b / gcd
    assert(gcd == 1)

    val lcmWithOffset = mod_floor(-x * bOffsetFromPerspectiveOfAOffset, b) * a

    (lcm, aOffset + lcmWithOffset)
  }


  def departWithOffsets(schedule: Seq[String]): BigInt =
    schedule
      .map(_.toIntOption)
      .zipWithIndex
      .filter(_._1.isDefined)
      .map(x => (BigInt(x._1.get), BigInt(x._2)))
      .reduce(leastCommonMultipleWithOffset)
      ._2


  println(departWithOffsets(busSchedule))
}
