import Lvl12.{ShipPart1, ShipPart2, inputToCommands}
import org.scalatest.flatspec.AnyFlatSpec

class lvl12Test extends AnyFlatSpec {
    private val input = """F10
                  |N3
                  |F7
                  |R90
                  |F11""".stripMargin.split("\r?\n")
    private val commands = inputToCommands(input)

  "part 1" should "calculate distance travelled" in {
    val ship = ShipPart1()
    val travelledShip = ship.executeAll(commands)
    val dist = travelledShip.distanceTravelled()
    assert(dist === 25)
  }

  "part 2" should "calculate distance travelled" in {
    val ship = ShipPart2()
    val travelledShip = ship.executeAll(commands)
    val dist = travelledShip.distanceTravelled()
    assert(dist === 286)
  }
}
