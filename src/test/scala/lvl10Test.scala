import Lvl10.{distinctArrangements, oneTimesThreeJoltDiffs}
import org.scalatest.flatspec.AnyFlatSpec

class lvl10Test extends AnyFlatSpec {
  private val example1_numbers = "16,10,15,5,1,11,7,19,6,12,4"
    .split(",")
    .map(_.toLong)

  private val example2_numbers = "28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3"
    .split(",")
    .map(_.toLong)

  "oneTimesThreeJoltDiffs" should "calculate product of 1-jolt and 3-jolt differences" in {
    assert(oneTimesThreeJoltDiffs(example1_numbers) === 7 * 5)
    assert(oneTimesThreeJoltDiffs(example2_numbers) === 22 * 10)
  }

  "distinctArangements" should "calculate the distinct number of adapter arrangements" in {
    assert(distinctArrangements(example1_numbers) === 8)
    assert(distinctArrangements(example2_numbers) === 19208)
  }
}
