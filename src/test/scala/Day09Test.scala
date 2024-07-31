import org.scalatest.funsuite.AnyFunSuite


class Day09Test extends AnyFunSuite{

    val day09Provided = new Day09(Seq[String]("London to Dublin = 464", "London to Belfast = 518", "Dublin to Belfast = 141"))

    test("distance between London and Belfast sholud be 518") {
        assert(day09Provided.distance("London", "Belfast") == 518)
    }

    test("Shortest route should be 605") {
        assert(day09Provided.part1() == 605)
    }

}
