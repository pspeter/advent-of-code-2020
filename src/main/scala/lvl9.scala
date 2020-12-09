import scala.io.Source

object lvl9 extends App {

  val numbers = Source.fromResource("lvl9.txt").getLines().map(_.toLong).toSeq

  // part 1
  def findNumThatIsNoSum(numbers: Seq[Long], preambleSize: Int = 25): Long =
    numbers
      .sliding(preambleSize + 1)
      .filter(window => {
        val preamble = window.init
        val newNum = window.last
        preamble.combinations(2).forall(comb => comb.sum != newNum)
      })
      .map(window => window.last)
      .next

  val invalidNum = findNumThatIsNoSum(numbers)
  println(s"Part 1 solution is: $invalidNum")

  // part 2
  def findEncryptionWeakness(numbers: Seq[Long], target: Long): Long =
    LazyList.from(2)
      .map(windowLen => numbers.sliding(windowLen).filter(_.sum == target))
      .filter(_.nonEmpty)
      .map(_.toSeq.head)
      .map(nums => nums.min + nums.max)
      .head

  val weakness = findEncryptionWeakness(numbers, target=invalidNum)

  println(s"Part 2 solution is: $weakness")
}
