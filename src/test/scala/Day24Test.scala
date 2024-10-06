import Day23.*
import org.scalatest.funsuite.AnyFunSuite


class Day24Test extends AnyFunSuite{
    val sampleInput: Seq[Int] = Seq(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)

    test("Half a") {
        val ben = Day24.smallestGroup(sampleInput)
        assert(ben.toSet.equals(Set(9, 11)))
    }


}
