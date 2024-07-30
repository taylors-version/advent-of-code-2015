import org.scalatest.funsuite.AnyFunSuite


class Day08Test extends AnyFunSuite{

    test("\"\" should return 2") {
        val input = Seq[String]("\"\"")
        assert(Day08.part1(input) == 2-0)
    }

    test("\"abc\" should return 2") {
        val input = Seq[String]("\"abc\"")
        assert(Day08.part1(input) == 5-3)
    }

    test("\"aaa\\\"aaa\" should return 3") {
        val input = Seq[String]("\"aaa\\\"aaa\"")
        assert(Day08.part1(input) == 10-7)
    }

    test("\"\\x27\" should return 5") {
        val input = Seq[String]("\"\\x27\"")
        assert(Day08.part1(input) == 6 - 1)
    }

    test("\"abc\", \"aaa\\\"aaa\" should return 5") {
        val input = Seq[String]("\"abc\"", "\"aaa\\\"aaa\"")
        assert(Day08.part1(input) == 5 )
    }

    test("\"njro\\x68qgbx\\xe4af\\\"\\\\suan\" should return 10") {
        val input = Seq[String]("\"njro\\x68qgbx\\xe4af\\\"\\\\suan\"")
        assert(Day08.part1(input) == 28 - 18)
    }

    test("\"\\\\\\\\mouqqcsgmz\" should return 4") {
        val input = Seq[String]("\"\\\\\\\\mouqqcsgmz\"")
        assert(Day08.part1(input) == 4)
    }

    test("\"\" should return 4"){
        val input = Seq[String]("\"\"")
        assert(Day08.part2(input) == 4)
    }

    test("\"aaa\\\"aaa\" should return 6") {
        val input = Seq[String]("\"aaa\\\"aaa\"")
        assert(Day08.part2(input) == 16-10)
    }

    test("\"\", \"aaa\\\"aaa\" should return 10") {
        val input = Seq[String]("\"\"", "\"aaa\\\"aaa\"")
        assert(Day08.part2(input) == 10)
    }

}
