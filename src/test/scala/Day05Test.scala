import org.scalatest.funsuite.AnyFunSuite


class Day05Test extends AnyFunSuite{

    test("a should return 0") {
        assert(Day05.part1("a") == 0)
    }

    test("aaa should return 1") {
        assert(Day05.part1("aaa") == 1)
    }

    test("haegwjzuvuyypxyu returns 0 because of xy") {
        assert(Day05.part1("haegwjzuvuyypxyu") == 0)
    }

    test("jchzalrnumimnmhp no double") {
        assert(Day05.repeatedLetter("jchzalrnumimnmhp") == false)
    }

    test("jchzalrnummimnmhp  double") {
        assert(Day05.repeatedLetter("jchzalrnummimnmhp") == true)
    }

    test("ugknbfddgicrmopn is nice, so returns 1"){
        assert(Day05.part1("ugknbfddgicrmopn") == 1)
    }
}
