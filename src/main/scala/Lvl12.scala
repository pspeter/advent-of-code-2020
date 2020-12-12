import scala.annotation.tailrec
import scala.io.Source

object Lvl12 extends App {
  val input = Source.fromResource("lvl12.txt").getLines()

  // part 1

  case class ShipPart1(facing: String = "E", location: (Int, Int) = (0, 0)) {
    val facings = Seq("N", "E", "S", "W")

    def move(command: MoveCommand): ShipPart1 =
      command match {
        case MoveCommand("N", distance) => ShipPart1(facing, (location._1 + distance, location._2))
        case MoveCommand("E", distance) => ShipPart1(facing, (location._1, location._2 + distance))
        case MoveCommand("S", distance) => ShipPart1(facing, (location._1 - distance, location._2))
        case MoveCommand("W", distance) => ShipPart1(facing, (location._1, location._2 - distance))
        case MoveCommand("F", distance) => move(MoveCommand(facing, distance))
      }

    def turn(command: Command): ShipPart1 = {
      val current_facing = facings.indexOf(facing)

      /** Mod where result is always positive */
      def mod_floor(a: Int, n: Int): Int = ((a % n) + n) % n

      val newFacing = command match {
        case TurnCommand("L", degrees) => facings(mod_floor(current_facing - degrees / 90, facings.length))
        case TurnCommand("R", degrees) => facings(mod_floor(current_facing + degrees / 90, facings.length))
      }

      ShipPart1(newFacing, location)
    }

    def execute(command: Command): ShipPart1 =
      command match {
        case TurnCommand(direction, degrees) => turn(TurnCommand(direction, degrees))
        case MoveCommand(direction, distance) => move(MoveCommand(direction, distance))
      }

    @tailrec
    final def executeAll(commands: Seq[Command]): ShipPart1 = {
      if (commands.isEmpty) return this
      val newShip = execute(commands.head)
      newShip.executeAll(commands.tail)
    }

    def distanceTravelled(): Int = location._1.abs + location._2.abs
  }

  trait Command {
    def action: String
    def value: Int
  }
  case class TurnCommand(action: String, value: Int) extends Command
  case class MoveCommand(action: String, value: Int) extends Command

  def inputToCommands(input: Iterable[String]): Seq[Command] = {
    val cmd_strs = input.filter(_.nonEmpty).map(_.splitAt(1))
    cmd_strs.map {
      case (cmd, i) if Seq("L", "R").contains(cmd) => TurnCommand(cmd, i.toInt)
      case (cmd, i) => MoveCommand(cmd, i.toInt)
    }.toSeq
  }

  val commands = inputToCommands(input.toSeq)
  val ship1 = ShipPart1()
  val travelledShip1 = ship1.executeAll(commands)
  val dist1 = travelledShip1.distanceTravelled()
  println(s"Solution part 1: $dist1")

  // part 2

  case class WayPoint(location: (Int, Int) = (1, 10)) {
    def turn(command: TurnCommand): WayPoint = {
      def rotatePoint(p: (Int, Int), degrees: Int): (Int, Int) =
        ((Math.cos(Math.toRadians(degrees)) * p._1 - Math.sin(Math.toRadians(degrees)) * p._2).round.toInt,
          (Math.sin(Math.toRadians(degrees)) * p._1 + Math.cos(Math.toRadians(degrees)) * p._2).round.toInt)

      command match {
        case TurnCommand("L", degrees) => WayPoint(rotatePoint(location, -degrees))
        case TurnCommand("R", degrees) => WayPoint(rotatePoint(location, degrees))
      }
    }
  }

    case class ShipPart2(waypoint: WayPoint = WayPoint((1, 10)), location: (Int, Int) = (0, 0)) {
    val facings = Seq("N", "E", "S", "W")

    def move(command: MoveCommand): ShipPart2 =
      command match {
        case MoveCommand("N", distance) => copy(waypoint=WayPoint(waypoint.location._1 + distance, waypoint.location._2))
        case MoveCommand("E", distance) => copy(waypoint=WayPoint(waypoint.location._1, waypoint.location._2 + distance))
        case MoveCommand("S", distance) => copy(waypoint=WayPoint(waypoint.location._1 - distance, waypoint.location._2))
        case MoveCommand("W", distance) => copy(waypoint=WayPoint(waypoint.location._1, waypoint.location._2 - distance))
        case MoveCommand("F", distance) => copy(location=(location._1 + waypoint.location._1 * distance, location._2 + waypoint.location._2 * distance))
      }

    def turn(command: TurnCommand): ShipPart2 = {
      copy(waypoint=waypoint.turn(command))
    }

    def execute(command: Command): ShipPart2 =
      command match {
        case TurnCommand(direction, degrees) => turn(TurnCommand(direction, degrees))
        case MoveCommand(direction, distance) => move(MoveCommand(direction, distance))
      }

    @tailrec
    final def executeAll(commands: Seq[Command]): ShipPart2 = {
      if (commands.isEmpty) return this
      val newShip = execute(commands.head)
      newShip.executeAll(commands.tail)
    }

    def distanceTravelled(): Int = location._1.abs + location._2.abs
  }

  val ship2 = ShipPart2()
  val travelledShip2 = ship2.executeAll(commands)
  val dist2 = travelledShip2.distanceTravelled()
  println(s"Solution part 1: $dist2")

}
