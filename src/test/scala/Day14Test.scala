import org.scalatest.funsuite.AnyFunSuite


class Day14Test extends AnyFunSuite{
    
    val testInput: Seq[String] = Seq[String]("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
      "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

    test("testInput for 1000 seconds = 1120") {
      assert(Day14.part1(testInput, 1000) == 1120)
    }

    test("testIput for 1000 seconds part2 = 689") {
      assert(Day14.part2(testInput, 1000) == 689)
    }
}
