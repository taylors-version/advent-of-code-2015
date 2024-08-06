import org.scalatest.funsuite.AnyFunSuite


class Day18Test extends AnyFunSuite{

    val testBoard: Seq[String] = Seq[String](
        ".#.#.#",
        "...##.",
        "#....#",
        "..#...",
        "#.#..#",
        "####.."
    )

    test("input should be 4") {
        assert(Day18.part1(testBoard, 4) == 4)
    }

}
