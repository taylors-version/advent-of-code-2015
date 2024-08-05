import org.scalatest.funsuite.AnyFunSuite


class Day16Test extends AnyFunSuite{

    test("Sue 4: perfumes: 2, vizslas: 0, cars: 6 fais") {
        assert(!Day16.meetsRequirements("Sue 4: perfumes: 2, vizslas: 0, cars: 6"))
    }

    test("Sue 5: pomeranians: 3, goldfish: 5, samoyeds: 2 passes") {
        assert(Day16.meetsRequirements("Sue 5: pomeranians: 3, goldfish: 5, samoyeds: 2"))
    }

    test("Sue 10: perfumes: 10, trees: 6, cars: 4 fails") {
        assert(!Day16.meetsRequirements("Sue 10: perfumes: 10, trees: 6, cars: 4"))
    }

}
