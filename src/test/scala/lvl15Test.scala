import Lvl15.startGameWith
import org.scalatest.flatspec.AnyFlatSpec

class lvl15Test extends AnyFlatSpec {
  "Part 1" should "solve example 1" in {
    val example = "0,3,6".split(",").toSeq.map(_.toInt)
    assert(startGameWith(example, 2020) === 436)
  }

  it should "sovle example 2" in {
    val example = "1,3,2".split(",").toSeq.map(_.toInt)
    assert(startGameWith(example, 2020) === 1)
  }

  it should "sovle example 3" in {
    val example = "3,1,2".split(",").toSeq.map(_.toInt)
    assert(startGameWith(example, 2020) === 1836)
  }

  "Part 2" should "solve example 1" in {
    val example = "0,3,6".split(",").toSeq.map(_.toInt)
    assert(startGameWith(example, 30000000) === 175594)
  }
}
