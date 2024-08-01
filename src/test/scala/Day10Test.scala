import org.scalatest.funsuite.AnyFunSuite


class Day10Test extends AnyFunSuite{

    test("1 should return 11") {
        assert(Day10.lookAndSay("1").equals("11"))
    }

    test("11 should return 21") {
        assert(Day10.lookAndSay("11").equals("21"))
    }

    test("21 should return 1211") {
        assert(Day10.lookAndSay("21").equals("1211"))
    }

    test("1121 should return 211211") {
        assert(Day10.lookAndSay("1121").equals("211211"))
    }

    test("1, 4 should return 111221") {
        assert(Day10.run("1", 4).equals("111221".length))
    }

    test("1, 5 should return 312211") {
        assert(Day10.run("1", 5).equals("312211".length))
    }

}
