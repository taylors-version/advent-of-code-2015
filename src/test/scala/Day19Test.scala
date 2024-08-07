import org.scalatest.funsuite.AnyFunSuite


class Day19Test extends AnyFunSuite{

    val test1Input: Seq[String] = Seq[String](
        "H => HO",
        "H => OH",
        "O => HH",
        ""
    )

    val test2Input: Seq[String] = Seq[String](
        "e => H",
        "e => O",
        "H => HO",
        "H => OH",
        "O => HH",
        ""
    )

    test("test1Input on 'HOH' should be 4") {
        val input = test1Input :+ "HOH"
        assert(Day19.part1(input) == 4)
    }

    test("test1Input on 'HOHOHO' should be 7") {
        val input = test1Input :+ "HOHOHO"
        assert(Day19.part1(input) == 7)
    }

    test("test2Input on 'HOH takes 3 steps"){
        val input = test2Input :+ "HOH"
        assert(Day19.part2(input) == 3)
    }

    test("test2Input on 'HOHOHO takes 6 steps") {
        val input = test2Input :+ "HOHOHO"
        assert(Day19.part2(input) == 6)
    }

}
