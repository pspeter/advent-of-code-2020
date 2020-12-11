import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Lvl10 extends App {
  val numbers = Source.fromResource("lvl10.txt").getLines().map(_.toLong).toSeq

  // Part 1
  def joltDifferences(adapters: Seq[Long]): Seq[Long] =
    adapters
      .sorted
      .prepended(0L)
      .sliding(2)
      .map{case Seq(lower, higher) => higher - lower}
      .toSeq
      .appended(3)

  def oneTimesThreeJoltDiffs(adapters: Seq[Long]): Int =
      joltDifferences(adapters)
        .groupBy(identity)
        .view.mapValues(_.size)
        .values
        .product


  println(s"Part 1 solution is: ${oneTimesThreeJoltDiffs(numbers)}")

  // Part 2
  // Slow brute force solution (never finishes with long input)
  def sumWithSuccessor(l: Seq[Long])(index: Int): Seq[Long] =
    l.take(index) :+ (l(index) + l(index + 1)) :++ l.takeRight(l.length - index - 2)

  def listPossibleSubArrangements(diffs: Seq[Long]): Set[Seq[Long]] = {
    diffs.indices.dropRight(1)
      .map(sumWithSuccessor(diffs))
      .toSet
      .filter(_.forall(_ <= 3))
      .flatMap(listPossibleSubArrangements)
      .incl(diffs)
  }

  def distinctArrangementsSlow(adapters: Seq[Long]): Long =
    listPossibleSubArrangements(joltDifferences(adapters)).size

  // fast recursive solution

  /** Function cache, similar to scalaz.Memo. Taken from https://stackoverflow.com/a/36960228/5266392 */
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  lazy val numPossibleArrangementsForNOnes: Int => BigInt = memoize {
    case 0 => 1
    case 1 => 1
    case 2 => 2
    case 3 => 4
    case n => numPossibleArrangementsForNOnes(n - 1) * 2 - numPossibleArrangementsForNOnes(n - 4)
  }

  // quicker solution based on consecutive 1s (assuming the difference is never 2 in the input, which holds true)
  def distinctArrangements(adapters: Seq[Long]): BigInt = {
    val d = joltDifferences(adapters).iterator
    var arrs = BigInt(1)
    while (d.hasNext) {
      val num = d.takeWhile(_ == 1).length
      arrs = arrs * numPossibleArrangementsForNOnes(num)
      d.dropWhile(_ != 1)
    }
    arrs
  }


  println(s"Part 2 solution is: ${distinctArrangements(numbers)}")
}

