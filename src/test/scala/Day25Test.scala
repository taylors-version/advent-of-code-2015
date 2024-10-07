import org.scalatest.funsuite.AnyFunSuite


class Day25Test extends AnyFunSuite{

    test("Triangle 1,1 = 1") {
        assert(Day25.stepNumber(1, 1) == 1)
    }

    test("Triangle 2,3 = 9") {
        assert(Day25.stepNumber(2, 3) == 9)
    }

}
