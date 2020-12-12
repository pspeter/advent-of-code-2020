import Lvl11.{iterateToEquilibrium, numOccupied, part1Rules, part2Rules}
import org.scalatest.flatspec.AnyFlatSpec

class lvl11Test extends AnyFlatSpec{
    private val input = """#.##.##.##
                          |#######.##
                          |#.#.#..#..
                          |####.##.##
                          |#.##.##.##
                          |#.#####.##
                          |..#.#.....
                          |##########
                          |#.######.#
                          |#.#####.##""".stripMargin.split("\r?\n").toSeq.map(_.split("").toSeq)

  "Part 1" should "count number of occupied seats after equilibrium" in {
    val finalState = iterateToEquilibrium(part1Rules)(input)
    assert(numOccupied(finalState) == 37)
  }

  "Part 2" should "count number of occupied seats after equilibrium" in {
    val finalState = iterateToEquilibrium(part2Rules)(input)
    assert(numOccupied(finalState) == 26)
  }
}
