import Lvl9.{findEncryptionWeakness, findNumThatIsNoSum}
import org.scalatest.flatspec.AnyFlatSpec

class lvl9Test extends AnyFlatSpec{
  private val numbers = "35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576"
    .split(",")
    .map(_.toLong)

  "findNumThatIsNoSum" should "find the number that is not a sum of two of the previous numbers" in {
    assert(findNumThatIsNoSum(numbers, 5) === 127)
  }

  "findEncryptionWeakness" should "find the weakness" in {
    assert(findEncryptionWeakness(numbers, target=127) === 62)
  }
}
