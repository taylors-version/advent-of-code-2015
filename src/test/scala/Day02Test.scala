import org.scalatest.funsuite.AnyFunSuite


class Day02Test extends AnyFunSuite{

    test("2x3x4 part1 should return 58 for sample1") {
        assert(Day02.part1("2x3x4".split("\n").toSeq) == 58)
    }

    test("2x3x4, 1x1x10 should return 101"){
        assert(Day02.part1("2x3x4\n1x1x10".split("\n").toSeq) == 101)
    }

    test("2x3x4 should return 34 for part2"){
        assert(Day02.part2("2x3x4".split("\n").toSeq)== 34)
    }

        test("2x3x4, 1x1x10 should return 48 for part2"){
        assert(Day02.part2("2x3x4\n1x1x10".split("\n").toSeq)== 48)
    }

}
