import Lvl14.{MaskChangeV1, MaskChangeV2, MemSet, memorySum, processInstructionsV1, processInstructionsV2, stringToMaskV1, stringToMaskV2}
import org.scalatest.flatspec.AnyFlatSpec

class lvl14Test extends AnyFlatSpec {
  private val input = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                        |mem[8] = 11
                        |mem[7] = 101
                        |mem[8] = 0""".stripMargin.split("\r?\n")

  private val commands = input.map{
    case s"mask = $mask" => MaskChangeV1(stringToMaskV1(mask))
    case s"mem[$address] = $value" => MemSet(address.toInt, value.toInt)
  }



  "Part 1" should "do memory stuff" in {
      val memory = processInstructionsV1(commands, Seq(), Map())
      val sumMemory = memorySum(memory)
      assert(sumMemory === 165)
  }

  "Part 2" should "do multiple assignment stuff" in {
      val input2 = """mask = 000000000000000000000000000000X1001X
                     |mem[42] = 100
                     |mask = 00000000000000000000000000000000X0XX
                     |mem[26] = 1""".stripMargin.split("\r?\n")

      val commands2 = input2.map{
        case s"mask = $mask" => MaskChangeV2(stringToMaskV2(mask))
        case s"mem[$address] = $value" => MemSet(address.toInt, value.toInt)
      }

      val memory = processInstructionsV2(commands2)
      val sumMemory = memorySum(memory)
      assert(sumMemory === 208)
  }
}
