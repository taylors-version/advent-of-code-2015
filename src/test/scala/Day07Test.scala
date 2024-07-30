import org.scalatest.funsuite.AnyFunSuite


class Day07Test extends AnyFunSuite{

    val emptyDay07 = new Day07(Seq[String]())

    test("0 should return 0") {
        val input = "0"
        assert(emptyDay07.componentFunction(Seq[Int](0), input) == 0)
    }

    test("1 should return 1") {
        val input = "1"
        assert(emptyDay07.componentFunction(Seq[Int](1), input) == 1)
    }

    test("NOT 1 should return 65534") {
        val input = "NOT 1"
        assert(emptyDay07.componentFunction(Seq[Int](1), input) == 65534)
    }

    test("NOT 2 should return 65533") {
        val input = "NOT 2"
        assert(emptyDay07.componentFunction(Seq[Int](2), input) == 65533)
    }

    test("123 AND 456 should return 72") {
        val input = "123 AND 456"
        assert(emptyDay07.componentFunction(Seq[Int](123, 456), input) == 72)
    }

    test("123 OR 456 should return 507") {
        val input = "123 OR 456"
        assert(emptyDay07.componentFunction(Seq[Int](123, 456), input) == 507)
    }

    test("123 LSHIFT 2 should return 492") {
        val input = "123 LSHIFT 2"
        assert(emptyDay07.componentFunction(Seq[Int](123, 2), input) == 492)
    }

    test("456 RSHIFT 2 should return 114") {
        val input = "456 RSHIFT 2"
        assert(emptyDay07.componentFunction(Seq[Int](456, 2), input) == 114)
    }

    test("123 -> x value is 123"){
        val input = "123 -> x"
        val Day07_123 = new Day07(Seq[String](input))
        assert(Day07_123.valueOfWire("x") == 123)
    }

    test("123 -> x\n456 -> y\nx AND y -> d, d is 72") {
        val input = "123 -> x\n456 -> y\nx AND y -> d"
        val Day07_123 = new Day07(input.split("\n"))
        assert(Day07_123.valueOfWire("d") == 72)
    }

    test("a AND b -> z\nd OR 123 -> a\nd OR 456 -> b\n5 -> d 77") {
        val input = "a AND b -> z\nd OR 123 -> a\nd OR 456 -> b\n5 -> d"
        val Day07_123 = new Day07(input.split("\n"))
        assert(Day07_123.valueOfWire("z") == 77)
    }

}
