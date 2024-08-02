import org.scalatest.funsuite.AnyFunSuite


class Day11Test extends AnyFunSuite{

    test("abbceffg doesn't have iol") {
        assert(Day11.iolTest("abbceffg"))
    }

    test("hijklmmn has iol") {
        assert(!Day11.iolTest("hijklmmn"))
    }

    test("abbceffg has two doubles"){
        assert(Day11.doubleTest("abbceffg"))
    }

    test("abbcegjk has only one double"){
        assert(!Day11.doubleTest("abbcegjk"))
    }

    test("hijklmmn has sequential letters"){
        assert(Day11.sequentialTest("hijklmmn"))
    }

    test("abbceffg fails sequential letters"){
        assert(!Day11.sequentialTest("abbceffg"))
    }

    test("abcdefgh next potential is abcdffaa"){
        assert(Day11.part1("abcdefgh").equals("abcdffaa"))
    }

}
