import org.scalatest.funsuite.AnyFunSuite


class Day07Test extends AnyFunSuite{

    test("0 should return 0") {
        val input = "0"
        assert(Day07.component(input) == 0)
    }

    test("1 should return 1") {
        val input = "1"
        assert(Day07.component(input) == 1)
    }

    test("NOT 1 should return 65534") {
        val input = "NOT 1"
        assert(Day07.component(input) == 65534)
    }

    test("NOT 2 should return 65533") {
        val input = "NOT 2"
        assert(Day07.component(input) == 65533)
    }

    test("123 AND 456 should return 72") {
        val input = "123 AND 456"
        assert(Day07.component(input) == 72)
    }

    test("123 OR 456 should return 507") {
        val input = "123 OR 456"
        assert(Day07.component(input) == 507)
    }

    test("123 LSHIFT 2 should return 492") {
        val input = "123 LSHIFT 2"
        assert(Day07.component(input) == 492)
    }

    test("456 RSHIFT 2 should return 114") {
        val input = "456 RSHIFT 2"
        assert(Day07.component(input) == 114)
    }

}
