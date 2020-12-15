import Lvl13.{departWithOffsets, departWithOffsetsBruteForce, departWithOffsetsBruteForce2, part1}
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration.DurationInt

class lvl13Test extends AnyFlatSpec {
  private val input = """939
                        |7,13,x,x,59,x,31,19""".stripMargin.split("\r?\n")
  private val earliestDeparture = input.head.toInt
  private val busSchedule = input.last.split(",")

  "Part 1" should "get the next available bus" in {
    val result = part1(earliestDeparture, busSchedule)
    assert(result === 295)
  }

  "Part 2" should "solve example 1" in {
    val result = departWithOffsets("17,x,13,19".split(","))
    assert(result === 3417)
  }

  it should "solve example 2" in {
    val result = departWithOffsets("67,7,59,61".split(","))
    assert(result === 754018)
  }

  it should "solve example 3" in {
    failAfter(1.second) {
      val result = departWithOffsets("1789,37,47,1889".split(","))
      assert(result === 1202161486)
    }
  }
}
