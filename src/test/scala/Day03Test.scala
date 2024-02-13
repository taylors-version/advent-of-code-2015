import org.scalatest.funsuite.AnyFunSuite


class Day03Test extends AnyFunSuite{

    test("> should return 2") {
        assert(Day03.part1(">") == 2)
    }

    test(">> should return 3") {
        assert(Day03.part1(">>") == 3)
    }

    test("^>v< should return 4") {
        assert(Day03.part1("^>v<") == 4)
    }

    test("^v should return 3 with robot") {
        assert(Day03.part2("<v") == 3)
    }

    test("^v^v^v^v^v should return 11 with robot") {
        assert(Day03.part2("^v^v^v^v^v") == 11)
    }
}
