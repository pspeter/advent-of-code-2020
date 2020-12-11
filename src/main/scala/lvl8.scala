import scala.language.implicitConversions
import enumeratum._

import scala.annotation.tailrec
import scala.io.Source

object Lvl8 extends App {

  sealed trait Operation extends EnumEntry
  object Operation extends Enum[Operation] {
    val values = findValues

    case object acc extends Operation
    case object jmp extends Operation
    case object nop extends Operation
  }

  case class Instruction(operation: Operation, argument: Int)

  object Instruction {
    implicit def stringToInstruction(s: String): Instruction = {
      val parts = s.split(" ")
      val op = parts(0)
      val arg = parts(1)
      Instruction(Operation.withName(op), arg.toInt)
    }
  }

  def parseInstructions(input: String) : List[Instruction] = input.split("\\r?\\n").map(Instruction.stringToInstruction).toList
  val instructions = Source.fromResource("lvl8.txt").getLines().map(Instruction.stringToInstruction).toList

  // part 1

  @tailrec
  def exec_v1(instructions: List[Instruction], execAt: Int = 0, visited: Set[Int] = Set(), acc: Int = 0): Int = {
    if (visited.contains(execAt) || execAt >= instructions.length) return acc
    instructions(execAt) match {
      case Instruction(Operation.acc, x) => exec_v1(instructions, execAt + 1, visited + execAt, acc + x)
      case Instruction(Operation.nop, _) => exec_v1(instructions, execAt + 1, visited + execAt, acc)
      case Instruction(Operation.jmp, x) => exec_v1(instructions, execAt + x, visited + execAt, acc)
    }
  }
  println(exec_v1(instructions))

  // part 2

  def exec_v2(instructions: List[Instruction], exec_at: Int = 0, visited: Set[Int] = Set(), acc: Int = 0, patched: Boolean = false): Either[Int, Int] = {
    if (visited.contains(exec_at) || exec_at > instructions.length)
      Left(acc)
    else if (exec_at == instructions.length)
      Right(acc)
    else instructions(exec_at) match {
      case Instruction(Operation.acc, x) => exec_v2(instructions, exec_at + 1, visited + exec_at, acc + x, patched)
      case Instruction(Operation.nop, _) => exec_v2(instructions, exec_at + 1, visited + exec_at, acc, patched) match {
        case Left(x) => if (!patched)
          exec_v2(instructions, exec_at + x, visited + exec_at, acc, patched = true)
        else Left(x)
        case Right(x) => Right(x)
      }
      case Instruction(Operation.jmp, x) => exec_v2(instructions, exec_at + x, visited + exec_at, acc, patched) match {
        case Left(x) => if (!patched)
          exec_v2(instructions, exec_at + 1, visited + exec_at, acc, patched = true)
        else Left(x)
        case Right(x) => Right(x)
      }
    }
  }

  println(exec_v2(instructions))

}
