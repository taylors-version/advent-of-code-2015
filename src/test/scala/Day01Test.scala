import org.scalatest.funsuite.AnyFunSuite


class Day01Test extends AnyFunSuite{

    test("Part 1 should return 0 for sample1") {
        assert(Day01.part1("(())") == 0)
    }

    test("Part 2 should handle sample input"){
        assert(Day01.part2("())(") == 3)
    }
}
