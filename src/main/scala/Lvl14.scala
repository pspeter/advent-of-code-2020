import scala.annotation.tailrec
import scala.io.Source
import scala.math.pow

object Lvl14 {
  private val Max36BitInt = pow(2L, 36).toLong - 1
  type MaskV1 = Seq[(Int, Long)]
  type Memory = Map[Long, Long]

  abstract class Instruction

  case class MaskChangeV1(mask: MaskV1) extends Instruction
  case class MaskChangeV2(mask: MaskV2) extends Instruction

  case class MemSet(address: Long, value: Long) extends Instruction

  case class MemSetFloating(address: Long, value: Long) extends Instruction

  def stringToMaskV1(m: String): MaskV1 =
    m.reverse.zipWithIndex.filter(_._1 != 'X').map(x => (x._1.toString.toInt, pow(2, x._2).toLong))

  def applyBit(bitAndPosition: (Int, Long), value: Long): Long = bitAndPosition match {
    case (0, bit) => value & (Max36BitInt - bit)
    case (1, bit) => value | bit
  }

  @tailrec
  def applyMask(mask: MaskV1, value: Long): Long =
    if (mask.isEmpty) value else applyMask(mask.tail, applyBit(mask.head, value))


  def applyInstructionVersion1(memory: Memory, mask: MaskV1, address: Long, value: Long): Memory =
    memory + (address -> applyMask(mask, value))


  @tailrec
  def processInstructionsV1(instructions: Seq[Instruction],
                            currentMask: MaskV1,
                            memory: Memory): Memory = {
    if (instructions.isEmpty) return memory
    val currentInstruction = instructions.head
    currentInstruction match {
      case MaskChangeV1(newMask) => processInstructionsV1(instructions.tail, newMask, memory)
      case MemSet(address, value) =>
        val updatedMemory = applyInstructionVersion1(memory, currentMask, address, value)
        processInstructionsV1(instructions.tail, currentMask, updatedMemory)
      case _ => throw new IllegalArgumentException("V1 instruction found in V2")
    }
  }


  def memorySum(memory: Memory): Long = memory.values.sum

  // part 2

  type MaskV2 = (Seq[Int], Long)

  def parseBinaryString(s: String): Long = {
    if (s.isEmpty) return 0
    val headValue = if (s.head == '0') 0 else pow(2, s.length - 1).toLong
    headValue + parseBinaryString(s.tail)
  }

  def stringToMaskV2(m: String): MaskV2 = {
    val xIndices = m.reverse.zipWithIndex.filter(_._1 == 'X').map(_._2)
    val forcedOnes = parseBinaryString(m.replace("X", "0"))
    (xIndices, forcedOnes)
  }

  def resolveFloatingBits(address: Long, xIndices: Seq[Int]) : Seq[Long] =
    if (xIndices.isEmpty) Seq(address) else
      Seq(resolveFloatingBits(address, xIndices.tail),
        resolveFloatingBits(address ^ pow(2, xIndices.head).toLong, xIndices.tail)).flatten


  def applyInstructionVersion2(memory: Memory, mask: MaskV2, address: Long, value: Long): Memory =
    memory ++ resolveFloatingBits(address | mask._2, xIndices=mask._1).map(_ -> value)

  @tailrec
  def processInstructionsV2(instructions: Seq[Instruction],
                            currentMask: MaskV2 = (Seq(), 0),
                            memory: Memory = Map()): Memory = {
    if (instructions.isEmpty) return memory
    val currentInstruction = instructions.head
    currentInstruction match {
      case MaskChangeV2(newMask) => processInstructionsV2(instructions.tail, newMask, memory)
      case MemSet(address, value) =>
        val updatedMemory = applyInstructionVersion2(memory, currentMask, address, value)
        processInstructionsV2(instructions.tail, currentMask, updatedMemory)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("lvl14.txt").getLines().toSeq
    val commands = input.map {
      case s"mask = $mask" => MaskChangeV1(stringToMaskV1(mask))
      case s"mem[$address] = $value" => MemSet(address.toInt, value.toInt)
    }

    val memory = processInstructionsV1(commands, Seq(), Map())
    val sumMemory = memorySum(memory)
    println(s"Solution part 1: $sumMemory")

    // part 2
    val commands2 = input.map {
      case s"mask = $mask" => MaskChangeV2(stringToMaskV2(mask))
      case s"mem[$address] = $value" => MemSet(address.toInt, value.toInt)
    }
    val memory2 = processInstructionsV2(commands2)
    val sumMemory2 = memorySum(memory2)
    println(s"Solution part 2: $sumMemory2")

  }
}
