import org.scalatest.funsuite.AnyFunSuite


class Day12Test extends AnyFunSuite{

    test("ab12cd-4ef = 8") {
        assert(Day12.part1("ab12cd-4ef") == 8)
    }

    test("part 2 [1,{\"c\":\"red\",\"b\":2},3] = 4") {
        assert(Day12.part2("[1,{\"c\":\"red\",\"b\":2},3]") == 4)
    }

    test("part 2 {\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5} = 0") {
        assert(Day12.part2("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") == 0)
    }

    test("part 2 [1,\"red\",5] = 6") {
        assert(Day12.part2("[1,\"red\",5]") == 6)
    }

}
