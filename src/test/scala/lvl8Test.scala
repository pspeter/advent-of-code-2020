import lvl8.{exec_v1, exec_v2, parseInstructions}
import org.scalatest.flatspec.AnyFlatSpec

class lvl8Test extends AnyFlatSpec {
      private val input = """nop +0
                             |acc +1
                             |jmp +4
                             |acc +3
                             |jmp -3
                             |acc -99
                             |acc +1
                             |jmp -4
                             |acc +6""".stripMargin

  "exec_v1" should "execute instructions" in {
    val instructions = parseInstructions(input)
    assert(exec_v1(instructions) === 5)
  }

  "exec_v2" should "patch and execute instructions" in {
    val instructions = parseInstructions(input)
    assert(exec_v2(instructions).getOrElse(-1) === 8)
  }
}
